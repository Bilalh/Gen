from lib import chain_lib
from lib import euclidean
from lib import domains
from lib import instances
from lib import limit
from lib import timeout

from abc import ABCMeta, abstractmethod
from datetime import datetime, timedelta
from pprint import pprint, pformat

import time
import calendar
import json
import logging
import os
import random
import math
import subprocess
from pathlib import Path


logger = logging.getLogger(__name__)


class Method(metaclass=ABCMeta):
    def __init__(self, options, limiter, setting_constructor, info):
        if "PARAM_GEN_SCRIPTS" not in os.environ:
            logger.error("$PARAM_GEN_SCRIPTS needs to defined")
            exit(2)

        if "NUM_JOBS" not in os.environ:
            logger.error("$NUM_JOBS needs to defined")
            exit(3)

        tools = os.path.join(os.path.expandvars("${PARAM_GEN_SCRIPTS}/"), "tools")

        if not os.path.isfile(os.path.join(tools, "cputimeout", "cputimeout" )):
            logger.error("Compile cputimeout in %s", os.path.join(tools, "cputimeout") )
            exit(4)

        self.info = info

        if options['output_dir']:
            self.output_dir = options['output_dir']
        else:
            self.output_dir = os.path.join(options['working_dir'], "out")

        os.makedirs(self.output_dir, exist_ok=True)

        domains.setup_domain(use_minion=options['use_minion'])
        instances.setup_instances(use_minion=options['use_minion'])

        for fp in ["info", "params"]:
            os.makedirs(os.path.join(self.output_dir, fp), exist_ok=True)

        if not options['seed']:
            options['seed'] = chain_lib.uniform_int(0, 2 ** 32)

        with open(os.path.join(self.output_dir, "info", "rerun_settings.json"), "w") as f:
            options['limiter'] = limiter.__class__.__name__
            options['info'] = self.info
            f.write(json.dumps(options))
            del options['limiter']
            del options['info']

        self.param_info = domains.gather_param_info(options['essence'], self.output_dir)
        logger.info(pformat(self.param_info, width=80))

        if len(self.info.ordering) != len(self.param_info):
            print("Ordering size:{} != params size:{}".format(len(self.info.ordering), len(self.param_info)))
            print("ordering:{}\nparam_info:§§{}".format(self.info.ordering, self.param_info))
            raise ValueError("invaild ordering")

        self.limiter = limiter

        options = self.before_settings(options)
        settings = setting_constructor(**options)
        logger.info(settings)
        self.settings = settings

        logger.info("Using Seed {}".format(settings.seed))
        random.seed(settings.seed)

        self.data_points = []
        self._current_iteration = 0
        self.prev_timestamp = None
        self.use_previous_data = True

        p_work = Path(self.settings.working_dir)
        self.models_dir = p_work / (p_work.name + "-" + self.settings.mode)
        self.num_models = chain_lib.get_number_of_models( str(self.models_dir) )
        logger.info(self.num_models)
        self.timeout = timeout.DynamicTimeout(self.settings.models_timeout, self.num_models, self)

        self.extra_time = 0
        if self.settings.use_minion:
            # TODO Should this be done before any script is run?
            self.generated_dir = os.path.join(self.output_dir, "generated")

            if settings.generated_dir:
                self.generated_dir = settings.generated_dir
                os.makedirs(self.generated_dir, exist_ok=True)

            os.makedirs(self.generated_dir, exist_ok=True)

            if Path(self.generated_dir, "essence_param_find.eprime").exists():
                logger.info("Not make essence of param, it exists")
            else:
                instances.create_param_essence(settings.essence, self.generated_dir)


            self.specific_dir = os.path.join(self.output_dir, "param_gen")
            os.makedirs(self.specific_dir, exist_ok=True)

            if settings.pre_generate:

                logger.info("(pre-)Genrating all solution using minion")
                (time_taken, solutions_counts) = instances.pre_create_all_param_solutions_from_essence(
                    self.generated_dir,  self.info.givens, self.param_info)
                self.solutions_counts = solutions_counts
                assert self.solutions_counts

                self.random_point = self.random_point_from_all_solutions_files
            else:
                # Uses minion to generate random points
                self.random_point = self.random_point_minion

        else:
            raise NotImplementedError("use_minion should be specifed")

    def run(self):
        date_start = datetime.utcnow()
        logger.info("Start %s", date_start.strftime("%a, %e %b %Y %H:%M:%S %s"))
        cpu_time_start = time.process_time()

        self.limiter.start()
        try:
            count_iter=True
            while self.limiter.continue_running(self, count_iter):
                count_iter = True
                logger.info("Started (real) iteration %d", self._current_iteration + 1)

                try:
                    self.do_iteration()
                except (domains.NoValuesInDomainException,
                        instances.FailedToGenerateParamExeception,
                        domains.DontCountIterationException) as e:
                    logger.info("Not counting this iter %d %s ", self._current_iteration, e)
                    count_iter=False

                self._current_iteration+=1
                logger.info("finished %d  on (real) iterations", self._current_iteration)

        except StopIteration:
            logger.info("StopIteration after/on iteration %d ", self._current_iteration)

        with open(os.path.join(self.output_dir, "info", "data-points.json"), "w") as f:
            f.write(json.dumps([  self.point_pretty(p) for p in self.data_points ]))

        cpu_time_end = time.process_time()
        date_end = datetime.utcnow()


        diff = datetime.utcnow() - date_start

        times = {
            "date_start": date_start.strftime("%a, %e %b %Y %H:%M:%S"),
            "date_end":   date_end.strftime("%a, %e %b %Y %H:%M:%S"),
            'time_stamp_start': date_start.timestamp(),
            'time_stamp_end': date_end.timestamp(),
            "real_time_formatted": str(diff),
            "real_time": diff.total_seconds(),
            "method_cpu_time": cpu_time_end- cpu_time_start,
            "method_extra_time": self.extra_time,
            "iterations_done_including_failed": self._current_iteration
        }


        if isinstance(self.limiter, limit.CpuLimit):
            times['cpu_time'] = self.limiter.taken
            times['cpu_time_given'] = self.settings.limit

        if isinstance(self.limiter, limit.TimeLimit):
            times['real_time_given'] = self.settings.limit

        for e in ['method_cpu_time', 'method_extra_time', 'cpu_time', 'cpu_time_given', 'real_time_given']:
            if e in times:
                td = timedelta(seconds=times[e])
                times[e +'_formatted'] = str(td)

        logger.info("Ω times %s", pformat(times))

        with (Path(self.output_dir) / 'times.json').open('w') as f:
            f.write(json.dumps(times))

    @abstractmethod
    def do_iteration():
        pass

    def before_settings(self, options):
        return options


    def create_run_param_and_store_quality(self, point):
        (param_string, param_name) = chain_lib.create_param_file(zip(self.info.ordering, point))
        return self.run_param_and_store_quality(param_string, param_name)

    def run_param_and_store_quality(self, param_string, param_name):
        logger.info((param_string, param_name))
        param_hash = chain_lib.hash(param_name)

        model_ordering = self.get_model_ordering()
        logger.info("eprime_ordering %s", model_ordering)

        check = self.use_previous(param_hash)
        time_per_model = self.timeout.time_per_model()
        if self.use_previous_data and check:
            now = check
            logger.info("using previous data timestamp %s", check)
        else:
            param_path = chain_lib.write_param(self.output_dir + "/params", param_string, param_hash)

            datee = calendar.datetime.datetime.now()
            logger.info("Start %s", datee.isoformat())
            now = str(int(datee.timestamp()))

            chain_lib.run_models(now, param_path, time_per_model, self.settings.working_dir,
                self.output_dir, self.settings.mode, model_ordering)
            logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )


        results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_hash,
            time_per_model, now, self.settings.mode)
        quailty = chain_lib.quality(*results)
        chain_lib.save_quality(self.output_dir, param_name, param_hash, quailty)

        self.prev_timestamp = now
        logger.info("results: {} quailty: {} for \n{}".format(results, quailty, param_string))
        return quailty


    def get_quailty(self, x):
        name = "-".join( [ ("%s" % p.safe) for p in x ] )
        name_hash = chain_lib.hash(name)
        logger.info("name %s for hash %s", name, name_hash)
        return chain_lib.get_quailty(self.output_dir, name_hash)


    def get_model_ordering(self):
        # self.models_dir
        import sqlite3
        if (Path(self.output_dir) / "results.db").exists():
            conn = sqlite3.connect(os.path.join(self.output_dir, 'results.db'))
            results = [  (self.models_dir / row[0]).with_suffix('.eprime')
                        for row in conn.execute("SELECT eprime FROM EprimeOrdering") ]
            if "LIMIT_MODELS" in os.environ:
                results = results[0:int(os.environ['LIMIT_MODELS'])]
            return "\n".join(map(str, results))
        else:
            return ""


    def use_previous(self, param_hash):
        import sqlite3
        if (Path(self.output_dir) / "results.db").exists():
            conn = sqlite3.connect(os.path.join(self.output_dir, 'results.db'))
            results = list(conn.execute("SELECT timestamp FROM Timeouts WHERE paramHash = ?", (param_hash,)))
            if len(results) == 0:
                return None
            return results[0][0]
        else:
            return None


    def random_point_from_all_solutions_files(self):
        num_solutions = self.solutions_counts[-1][0]
        u = chain_lib.uniform_int(1, num_solutions)
        logger.info( pformat(self.solutions_counts) )
        logger.info('picked solution %d', u)
        # calcuate the file  and line number that the solution is in

        for (i, (index, _) ) in enumerate(self.solutions_counts):
            if index >= u:
                solution_path = self.solutions_counts[i - 1][1]
                line_index = u - self.solutions_counts[i - 1][0]
                break

        logger.info('solution_path %s line_index %d', solution_path, line_index)

        all_sol_path = os.path.join("all_sols", solution_path)

        logger.info("all_sol_path %s", all_sol_path)
        split_test="{}.{:010d}".format(all_sol_path, 0)
        logger.info("all_sol_path split_test %s", split_test)
        if (Path(self.generated_dir) / split_test).exists():
            logger.info("Using split files for %d %s", line_index, solution_path)
            # command used split them (need gsplit on mac)
            # parallel -j8  "split -d -a10 -l 1000000 {} {}. " ::: *.minion-solution
            # numbed from 0000000000

            # since there are 1000000 lines in the file
            file_index = (line_index -1) // 1000000
            if line_index == 1000000:
                line_index = 0
            else:
                line_index = line_index % 1000000

            all_sol_path += ".%010d" % file_index
            logger.info('new solution_path line_index %d @ %s', line_index, all_sol_path)
        else:
            logger.warning('%s, is not split', solution_path)

        sol_line = subprocess.check_output(["sed", "{}q;d".format(line_index), all_sol_path ],
            universal_newlines=True, cwd=self.generated_dir)


        p = Path(self.specific_dir) / solution_path
        sol_path = p.with_suffix(".solution.%d" % line_index)
        with sol_path.open('w') as f:
            f.write(sol_line)

        minion=str((Path('all_sols') / solution_path).with_suffix('.minion'))
        eprime_param=str((Path('all_sols') / solution_path).with_suffix('.eprime-param'))

        eprime_solution=str(p.with_suffix(".eprime-solution.%d" % line_index))
        solution=str(p.with_suffix(".solution.%d" % line_index))
        solution_json=p.with_suffix(".json.%d" % line_index)

        start_usr = os.times().children_user
        start_sys = os.times().children_system

        arr=[
            'savilerow', '-mode', 'ReadSolution',
            '-in-eprime', 'essence_param_find.eprime',
            '-out-minion', minion,
            '-minion-sol-file', str(sol_path),
            '-out-solution', eprime_solution ]
        print(" ".join(arr) )

        subprocess.check_call(cwd=self.generated_dir, args=arr)

        arr=[
            "conjure", "--mode", "translateSolution",
            "--in-eprime", 'essence_param_find.eprime',
            "--in-essence", 'essence_param_find.essence',
            '--in-eprime-solution', eprime_solution,
            '--in-eprime-param', eprime_param,
            '--out-solution', solution ]
        print(" ".join(arr) )

        subprocess.check_call(cwd=self.generated_dir, args=arr)

        # add the param to the solution param
        subprocess.check_call(cwd=self.generated_dir, shell=True,
            args="cat '{}' | sed '1d' >> '{}'".format(
                (Path(self.generated_dir) / 'all_sols_params' / p.stem).with_suffix('.param'), solution
        ))

        subprocess.check_call([
            'essenceLettingsToJson', solution, str(solution_json)
        ])

        end_usr = os.times().children_user
        end_sys = os.times().children_system

        # reports 0 on windows
        cputime_taken = (end_usr - start_usr) + (end_sys - start_sys)

        self.extra_time += cputime_taken
        logger.info("Took %0.2f to get a solution (total %0.2f)", cputime_taken, self.extra_time )

        with solution_json.open() as f:
            raw_json = json.loads(f.read())

        param_map = dict([ instances.json_to_param_instance(letting) for letting in raw_json['lettings'] ])

        return [  param_map[name] for name in self.info.ordering ]

    def random_point_minion(self):
        selected_vals = {}

        for name in self.info.givens:
            v=self.param_info[name].random_value(selected_vals)
            selected_vals[name] = v
            logger.info("Assigning %s=%s", name, v.pretty)

        givens = [  (name, selected_vals[name]) for name in self.info.givens ]
        (generated, cputime_taken) = instances.create_param_from_essence(self.specific_dir, self.generated_dir, givens)
        self.extra_time += cputime_taken

        selected_vals.update(generated)
        return [  selected_vals[name] for name in self.info.ordering ]


    # FIXME
    def do_radius_as_percentage(self, options):
        if options['radius_as_percentage']:
            raise NotImplementedError("only works for int")
        else:
            self.shape = euclidean

        return options


    def point_pretty(self, x):
        return [(n, y.pretty) for (n, y) in zip(self.info.ordering, x) ]
