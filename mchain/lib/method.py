from lib import chain_lib
from lib import ncube
from lib import ncuboid
from lib import euclidean
from lib import domains
from lib import constraints
from lib import instances
from lib import limit

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
import sys
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

        if not os.path.isfile(os.path.join(tools, "timeout5")):
            logger.error("Compile timeout5 in %s", tools )
            exit(5)

        self.info = info

        if options['output_dir']:
            self.output_dir = options['output_dir']
        else:
            self.output_dir = os.path.join(options['working_dir'], "out")

        os.makedirs(self.output_dir, exist_ok=True)

        domains.setup_domain(use_minion=options['use_minion'])

        for fp in ["info", "params", 'param_gen']:
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
            raise ValueError()

        self.limiter = limiter

        options = self.before_settings(options)
        settings = setting_constructor(**options)
        logger.info(settings)
        self.settings = settings

        #FIXME hard coded
        if self.settings.essence.endswith("prob013-PPP.essence"):
            self.param_info['crew'].constraints.append(constraints.FuncForallLessThenEq('capacity'))

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
        self.time_per_model = int(math.ceil(self.settings.models_timeout / self.num_models) )

        if self.settings.use_minion:
            # FIXME this should be create once for ALL methods
            instances.create_param_essence(options['essence'], self.output_dir)
            # Uses minion to generate random points
            self.random_point = self.random_point_minion
        else:
            # Generate the params ourselves
            self.random_point = self.random_point_generated

        # self.random_point = self.random_point_genrated
        self.extra_time = 0

    def run(self):
        date_start = datetime.utcnow()
        logger.info("Start %s", date_start.strftime("%a, %e %b %Y %H:%M:%S %s"))
        cpu_time_start = time.process_time()

        self.limiter.start()
        try:
            while self.limiter.continue_running(self):
                logger.info("Started iteration %d", self._current_iteration + 1)
                self.do_iteration()
                self._current_iteration+=1
                logger.info("finished %d iterations", self._current_iteration)
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
            "iterations_done": self._current_iteration
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
        if self.use_previous_data and check:
            now = check
            logger.info("using previous data timestamp %s", check)
        else:
            param_path = chain_lib.write_param(self.output_dir + "/params", param_string, param_hash)

            datee = calendar.datetime.datetime.now()
            logger.info("Start %s", datee.isoformat())
            now = str(int(datee.timestamp()))

            chain_lib.run_models(now, param_path, self.time_per_model, self.settings.working_dir, self.output_dir, self.settings.mode, model_ordering)
            logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )


        results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_hash, self.time_per_model, now, self.settings.mode)
        quailty = chain_lib.quality(*results)
        chain_lib.save_quality(self.output_dir, param_name, param_hash, quailty)

        self.prev_timestamp = now
        logger.info("results: {} quailty: {} for \n{}".format(results, quailty, param_string))
        return quailty

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

    def random_point_minion(self):
        selected_vals = {}

        for name in self.info.givens:
            v=self.param_info[name].random_value(selected_vals)
            selected_vals[name] = v
            logger.info("Assigning %s=%s", name, v.pretty)

        givens = [  (name, selected_vals[name]) for name in self.info.givens ]
        (generated, cputime_taken) = instances.create_param_from_essence(self.output_dir, givens)
        self.extra_time += cputime_taken

        selected_vals.update(generated)
        return [  selected_vals[name] for name in self.info.ordering ]

    def random_point_generated(self):
        selected_vals = {}

        for name in self.info.ordering:
            v=self.param_info[name].random_value(selected_vals)
            selected_vals[name] = v
            logger.info("Assigning %s=%s", name, v.pretty)

        return [  selected_vals[name] for name in self.info.ordering ]


    # FIXME
    def do_radius_as_percentage(self, options):
        if options['radius_as_percentage']:
            raise NotImplementedError("only works for int")
            self.shape = ncuboid
            for s in ['select_radius', 'influence_radius']:
                if s in options:
                    per = options[s]
                    radii = [ math.ceil((u - l) * (per / 100)) for (l, u) in self.data ]
                    options[s] = radii
        else:
            self.shape = euclidean

        return options


    def point_pretty(self, x):
        return [(n, y.pretty) for (n, y) in zip(self.info.ordering, x) ]
