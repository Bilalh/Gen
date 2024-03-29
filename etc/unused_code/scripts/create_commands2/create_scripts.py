#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import calendar
import itertools
import json
import logging
import os
import re
import sqlite3
import sys
import uuid
import random
import math

from distutils import dir_util
from distutils import file_util
from itertools import groupby
from pathlib import Path
from pprint import pprint

logger = logging.getLogger(__name__)
prog_name = os.path.dirname(sys.argv[0])
prog_dir = os.path.abspath(prog_name)

# Arguments which are not passed to the methods as --flags
MetaFields = {'per_model_time', 'per_race_time', 'num_models',
              'run_no', 'filepath', 'way',  'iterations', 'method',
              "uuid", "id", 'gid', 'guuid', 'total_time'}

Fields = sorted({'chain_length', 'essence', 'influence_radius', 'mode',
                'num_points', 'output_dir', 'point_selector', 'pre_generate',
                'radius_as_percentage', 'select_radius', 'timeout', 'use_minion',
                'working_dir', 'seed'} | MetaFields)

ToRemove = { 'limit', 'directory', 'filepath' }

# id should be unique, uuid is unique



def run(fp, place_dir, num_runs):
    data = read_json(fp)
    for p in data['essences']:
        p['directory'] = Path(p['directory'])

    essences_dir = place_dir / "results" / "specs"
    if not essences_dir.exists():
        essences_dir.mkdir(parents=True)

    datee = calendar.datetime.datetime.now()
    now = str(int(datee.timestamp()))
    ffp=Path(fp)


    create_essence_metadata(place_dir)
    # Create own own copy of the essence and the files we need
    for values in data['essences']:
        essence_dir = essences_dir / values['directory'].name
        if not essence_dir.exists():
            essence_dir.mkdir(parents=True)
        essence_name = essence_dir.name

        eprimes_dirname = essence_dir.name + "-" + values['mode']
        eprimes_dir = essence_dir / eprimes_dirname

        essence_path = (values['directory'] / essence_name).with_suffix('.essence')
        results_essence = (essence_dir / essence_name).with_suffix('.essence')

        for f in ['params.pcs', 'info.json',
                    essence_name + ".essence", essence_name + ".essence.givens" ]:
            copy_file(values['directory'] / f, essence_dir / f)

        dir_util.copy_tree( str(values['directory'] / eprimes_dirname), str(eprimes_dir))
        values['num_models'] = len(list(eprimes_dir.glob('*.eprime')))

        values['directory'] = Path("results") / "specs" / values['directory'].name
        values['filepath'] = (values['directory'] / essence_name).with_suffix('.essence')
        values['essence'] = essence_name

        insert = "INSERT INTO essences(essence, mode, num_models, filepath) VALUES(?, ?, ?, ?)"
        with sqlite3.connect(str(place_dir / "results" / "Info.db")) as conn:
            conn.execute(insert,
                (essence_name, values['mode'], values['num_models'], str(values['filepath'])))
            conn.commit()
        conn.close()


        results = for_methods(data, values, place_dir, num_runs)
        end={  k: [  make_script_from_data(k, v) for v in vv ] for (k, vv) in results.items()  }

        cmd_file = place_dir / "results" / values['directory'].name / "run_commands.sh"
        with cmd_file.open("w") as f:
            f.write("\n".join( itertools.chain(*end.values()) ))
        cmd_file.chmod(0o755)


        init_file = place_dir / "results" / "init.sh"
        copy_file(Path(prog_dir) / "init.sh", init_file )
        init_file.chmod(0o755)
        with init_file.open("a") as f:
            f.write("\nexport JAVA_MEMORY=%s\n" % data['JAVA_MEMORY'])


        copy_file(Path(prog_dir) / "show_unfinished.py", place_dir / "results" / "show_unfinished.py" )
        (Path(prog_dir) / "show_unfinished.py").chmod(0o755)

        if values['num_models'] < data['cores']:
            jobs = values['num_models']
        else:
            jobs = data['cores']

        # parallel --header : --colsep '\t' echo a={a} b={b} c={c} :::: a.tsv

        log_file = place_dir / "results" / values['directory'].name / "_logfile"
        run_file = place_dir / "results" / values['directory'].name / "run.sh"
        with run_file.open("w") as f:
            lines = [
                "#!/bin/bash",
                "# requires python3.3+",
                "export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
                "export NUM_JOBS={}".format(jobs),
                "",
                ". " + str(init_file),
                "cat %s | parallel -j${CONCURRENT_RUNS:-1}  --halt 2 --joblog %s ${MAIN_PARALLEL_ARGS} " % (cmd_file, log_file),
                ""
            ]
            f.write("\n".join(lines))
        run_file.chmod(0o755)

    def write_run_all(path, resume=False):
        with path.open("w") as f:
            f.write("#!/bin/bash\n")
            if resume:
                f.write("export MAIN_PARALLEL_ARGS=--resume;\n")
            f.write("""
                _start=`date`;
                export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/;
            """)
            f.write("\n".join(
                "    ./%s;" % s.relative_to(place_dir) for s in
                    sorted(place_dir.glob('results/*/run.sh')  )))
            f.write("""
                return_code=$?;
                _end=`date`; echo $_start  $_end | tee "{total_time_file}_`date +_%s`";
                if [ $return_code == 0 ]; then
                     $PARAM_GEN_SCRIPTS/gent/hitting_set_gent.sh {results} {results}/__gent;
                fi
            """.format(
                total_time_file=(place_dir / "results" / "total_time"),
                results=place_dir / "results"
                ))

        path.chmod(0o755)


    run_all_file = place_dir / "results" / "run_all.sh"
    run_all_resume_file = place_dir / "results" / "run_all_resume.sh"
    write_run_all(run_all_file)
    write_run_all(run_all_resume_file, resume=True)

    copy_file(ffp, place_dir / "results" / ("settings-" + now + ".json") )




def for_methods(data, es, place_dir, num_runs):
    essence_results = place_dir / "results" / es['directory'].name
    if not essence_results.exists(): essence_results.mkdir(parents=True)

    remove_run_no=re.compile(r"__\d+__")

    gids = {}

    def f(method_name):

        if 'iterations' in data and 'cpu' in data:
            logger.error("Can only have on of {iterations, cpu}")
            sys.exit(9)
        if 'iterations' in data:
            args = data['iterations'].copy()
            extra_fmt="{method}_{mode}_{per_model_time}_i{iterations}__{run_no}__"
        elif 'cpu' in data:
            args = data['cpu'].copy()
            extra_fmt="{method}_{mode}_{per_model_time}_t{total_time}__{run_no}__"
        else:
            logger.error("need iterations or cpu")
            sys.exit(3)

        args['run_no'] = list(range(1, num_runs + 1))

        args.update(es)
        args.update(data[method_name])

        all_combs=producter(args)
        for a in all_combs:
            a['method']  = method_name

            extra = extra_fmt.format(**a)
            extra += "_".join( str(a[k]) for k in sorted(a.keys())
                if k in data[method_name].keys() )
            a['id'] = "{}_{}".format(a['essence'], extra)
            a['uuid'] = uuid.uuid4().hex

            a['gid'] = remove_run_no.sub("_", a['id'])
            if a['gid'] not in gids:
                gids[ a['gid'] ] = uuid.uuid4().hex
            a['guuid'] = gids[ a['gid'] ]

            a['output_dir'] = Path('results') / a['directory'].name / ("out_" + extra)

            a['per_race_time'] = a['num_models'] *  a['per_model_time']

            a['seed'] = math.ceil(random.uniform(-1, 2**24))

            if 'iterations' in args:
                a['limit'] = a['iterations']
                a['way'] = "iterations"
            else:
                a['limit'] = a['total_time']
                a['way'] = "cpu"

        return all_combs

    results = { k : f(k) for k in ['nsample', 'ksample', 'uniform'] if k in data }

    return results


def make_script_from_data(name, data):
    scripts ={
        "nsample" : "../instancegen/mchain/nsampling.py",
        "uniform" : "../instancegen/mchain/uniform_sampling.py",
        "ksample" : "../instancegen/mchain/ksampling.py"
    }

    output="record_cp {output_dir}/logs/log {script} {way} {limit} ".format(
        script=scripts[name], **data)
    output += " ".join( " --{}={:6}".format(k, v) for (k, v) in process_args(data).items() )


    ndata = data.copy()
    ndata['working_dir'] = ndata['directory']

    keys = sorted(ndata.keys())
    not_storing = {'limit', 'directory', 'filepath'}

    def t(v):
        if isinstance(v, bool):
            return int(v)
        elif isinstance(v, int):
            return v
        else:
            return "'%s'" % v

    output += " && [ -f '{output_dir}/times.json' ] && printf \".timeout 5000\\nINSERT INTO everything({}) VALUES({});\"".format(
            ", ".join("'{}'".format(k) for k in keys if k not in not_storing ),
            ", ".join("{:5}".format(t(ndata[k])) for k in keys if k not in not_storing),
            **data
        )
    output += " | sqlite3 results/Info.db "
    output += " && $PARAM_GEN_SCRIPTS/misc/tar_results.sh {output_dir} {mode} ".format(**ndata)
    output += ";"
    return output


# Keep only the needed fields for the method
def process_args(args1):
    args = args1.copy()
    args['models_timeout'] = args['per_race_time']
    args['working_dir'] = args['directory']
    args['generated_dir'] = args['working_dir'] / "generated"
    args['info'] = args['working_dir'] / "info.json"
    args['essence'] =args['filepath']


    for d in MetaFields | ToRemove:
        if d in args: del args[d]

    return { k:str(v) for (k, v) in args.items()}


def producter(common):
    def arr(vs):
        if isinstance(vs, list):
            return vs
        return [vs]

    zipped =[ [(k, v) for v in arr(vs)] for (k, vs) in common.items() ]
    return [dict(kv) for kv in itertools.product(*zipped)]


def create_essence_metadata(place_dir):
    query = """
    CREATE TABLE IF NOT EXISTS  "essences" (
        essence TEXT,
        mode TEXT,
        num_models INTEGER,
        filepath TEXT,
        PRIMARY KEY(essence, mode)
    );
    """
    conn = sqlite3.connect(str(place_dir /  "results" /  "Info.db"))
    conn.execute(query)

    fields_to_use = set(Fields) - ToRemove


    order_first =[ 'method', 'essence', 'mode', 'num_models', 'way', 'run_no', 'per_model_time', 'per_race_time', 'total_time']

    if set(order_first) <= fields_to_use:
        fields_to_use = fields_to_use - set(order_first)
        fields_to_use = order_first + sorted(fields_to_use)

    everything="""
        CREATE TABLE IF NOT EXISTS  "everything" (%s ,PRIMARY KEY(%s) );
    """ % (
        ",".join("{}".format(f)  for f in fields_to_use ) ,
        ",".join(map(str, fields_to_use))
    )
    conn.execute(everything)


    conn.commit()
    conn.close()

def copy_file(path1, path2):
    file_util.copy_file(str(path1), str(path2))


def read_json(fp):
    os.path.abspath(os.path.expanduser(fp))
    with open( fp ) as f:
        json_in = f.read()
    return json.loads(json_in)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("json_settings")
    parser.add_argument("output_dir")
    parser.add_argument("num_runs", type=int)
    args = parser.parse_args()
    run(args.json_settings,
        Path(args.output_dir),
        args.num_runs)

