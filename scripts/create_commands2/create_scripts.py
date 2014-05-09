#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import calendar
import itertools
import json
import logging
import os
import sys
import sqlite3

from distutils import dir_util
from distutils import file_util
from itertools import groupby
from pathlib import Path
from pprint import pprint

logger = logging.getLogger(__name__)
prog_name = os.path.dirname(sys.argv[0])
prog_dir = os.path.abspath(prog_name)

Fields = sorted({'essence', 'mode', 'num_models', 'per_race_time',
        'radius_as_percentage', 'use_minion', 'pre_generate', 'output_dir',
        'iterations', 'run_no', 'per_model_time', 'method', 'influence_radius',
        'way', 'essence', 'point_selector', 'num_points', 'chain_length',
        'select_radius', 'working_dir', 'dynamic_timeout', 'races'})


def run(fp, place_dir, num_runs):
    data = read_json(fp)
    for p in data['essences']:
        p['directory'] = Path(p['directory'])

    essences_dir = place_dir / "results" / "specs"
    essences_dir.mkdir(parents=True)

    datee = calendar.datetime.datetime.now()
    now = str(int(datee.timestamp()))
    ffp=Path(fp)
    copy_file(ffp, place_dir / "results" / ("settings-" + now + ".json") )


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

        # parallel --header : --colsep '\t' echo a={a} b={b} c={c} :::: a.tsv

        insert = "INSERT INTO essences(essence, mode, num_models, filepath) VALUES(?, ?, ?, ?)"
        with sqlite3.connect(str(place_dir / "results" / "Info.db")) as conn:
            conn.execute(insert,
                (essence_name, values['mode'], values['num_models'], str(essence_path)))
            conn.commit()
        conn.close()

        values['directory'] = Path("results") / "specs" / values['directory'].name
        values['filepath'] = (values['directory'] / essence_name).with_suffix('.essence')
        values['essence'] = essence_name

        results = for_methods(data, values, place_dir, num_runs)
        end=[  [  make_script_from_data(k, v) for v in vv ] for (k, vv) in results.items() ]

        cmd_file = place_dir / "results" / values['directory'].name / "run_commands.sh"
        with cmd_file.open("w") as f:
            f.write("\n".join(end[0]))
        cmd_file.chmod(0o755)

        init_file = place_dir / "results" / "init.sh"
        copy_file(Path(prog_dir) / "init.sh", init_file )
        init_file.chmod(0o755)
        with init_file.open("a") as f:
            f.write("\nexport JAVA_MEMORY=%s\n" % data['JAVA_MEMORY'])

        if values['num_models'] < data['cores']:
            jobs = values['num_models']
        else:
            jobs = data['cores']

        run_file = place_dir / "results" / values['directory'].name / "run.sh"
        with run_file.open("w") as f:
            lines = [
                "#!/bin/bash",
                "# requires python3.3+",
                "export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
                "export NUM_JOBS={}".format(jobs),
                "",
                ". " + str(init_file),
                str(cmd_file),
                ""
            ]
            f.write("\n".join(lines))
        run_file.chmod(0o755)


    run_all_file = place_dir / "results" / "run_all.sh"
    with run_all_file.open("w") as f:
        f.write("#!/bin/bash\n")
        f.write("_start=`date` \n")
        f.write("\n".join(
            "./%s" % s.relative_to(place_dir) for s in
                sorted(place_dir.glob('results/*/run.sh')  )))

        f.write("\n_end=`date`; echo $_start  $_end | tee %s\n"
            % (place_dir / "results" / "total_time") )

    run_all_file.chmod(0o755)


def for_methods(data, es, place_dir, num_runs):
    essence_results = place_dir / "results" / es['directory'].name
    if not essence_results.exists(): essence_results.mkdir(parents=True)

    def f(method_name):
        args = data['iterations'].copy()
        args['run_no'] = list(range(1, num_runs + 1))

        args.update(es)
        args.update(data[method_name])

        all_combs=producter(args)
        for a in all_combs:
            extra = "out_{mode}_{per_model_time}_{iterations}__{run_no}__".format(**a)
            extra += "_".join( str(v) for (k, v) in a.items() if k in data['nsample'].keys() )
            a['output_dir'] = Path('results') / a['directory'].name / extra

            a['per_race_time'] = a['num_models'] *  a['per_model_time']

            a['method']  = method_name
            if 'iterations' in args:
                a['limit'] = a['iterations']
                a['way'] = "iterations"

        return all_combs

    results = { k : f(k) for k in ['nsample', 'uniform'] if k in data }

    return results


def make_script_from_data(name, data):
    scripts ={
        "nsample" : "../instancegen/mchain/nsampling.py",
        "uniform" : "../instancegen/mchain/uniform_sampling.py",
        "ksample" : "../instancegen/mchain/ksample.py"
    }

    output="record_cp {output_dir}/logs/log {script} {way} {limit} ".format(
        script=scripts[name], **data)
    output += " ".join( " --{}={:6}".format(k, v) for (k, v) in process_args(data).items() )


    ndata = data.copy()
    ndata['essence'] = ndata['filepath']
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

    output += " && printf \".timeout 5000\\nINSERT INTO everything({}) VALUES({});\"".format(
            ", ".join("'{}'".format(k) for k in keys if k not in not_storing ),
            ", ".join("{:5}".format(t(ndata[k])) for k in keys if k not in not_storing),
        )
    output += " | sqlite3 results/Info.db"
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


    for d in ['per_model_time', 'per_race_time', 'directory', 'num_models',
              'run_no', 'filepath', 'way', 'limit', 'iterations', 'method']:
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

    everything="""
        CREATE TABLE IF NOT EXISTS  "everything" (%s ,PRIMARY KEY(%s) );
    """ % (
        ",".join("{}".format(f)  for f in Fields ),
        ",".join(map(str, Fields))
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

