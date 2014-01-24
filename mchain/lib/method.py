from lib import chain_lib
from lib import ncube
from lib import ncuboid
from lib import domain

from abc import ABCMeta, abstractmethod
from datetime import datetime
from pprint import pprint, pformat

import time
import calendar
import json
import logging
import os
import random
import math
import sys

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

        self.param_info = domain.gather_param_info(options['essence'], self.output_dir)
        logger.info(pformat(self.param_info, width=80))


        if len(self.info.ordering) != len(self.param_info):
            print("Ordering size:{} != params size:{}".format(len(self.info.ordering), len(self.param_info)))
            print("ordering:{}\nparam_info:§§{}".format(self.info.ordering, self.param_info))
            sys.exit(8)

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


    def run(self):
        date_start = datetime.utcnow()
        logger.info("Start %s", date_start.strftime("%a, %e %b %Y %H:%M:%S %s"))
        cpu_time_start = time.process_time()

        self.limiter.start()
        while self.limiter.continue_running(self):
            self.do_iteration()
            self._current_iteration+=1

        with open(os.path.join(self.output_dir, "info", "data-points.json"), "w") as f:
            f.write(json.dumps(self.data_points))

        cpu_time_end = time.process_time()
        date_end = datetime.utcnow()
        logger.info("Ω Start %s", date_start.strftime("%a, %e %b %Y %H:%M:%S %s"))
        logger.info("Ω End %s", date_end.strftime("%a, %e %b %Y %H:%M:%S %s"))

        diff = datetime.utcnow() - date_start
        logger.info("Ω Total real time %s", diff)
        logger.info("Ω Total real time(seconds) %s", diff.total_seconds())
        logger.info("Ω Our cpu time %0.4f",  cpu_time_end- cpu_time_start )


    @abstractmethod
    def do_iteration():
        pass

    def before_settings(self, options):
        return options

    def run_param_and_store_quality(self, point):
        (param_string, param_name) = chain_lib.create_param_essence(zip(self.info.ordering, point))
        logger.info((param_string, param_name))
        param_path = chain_lib.write_param(self.output_dir, param_string, param_name)

        datee = calendar.datetime.datetime.now()
        logger.info("Start %s", datee.isoformat())
        now = str(int(datee.timestamp()))

        chain_lib.run_models(now, param_path, self.settings.models_timeout, self.settings.working_dir, self.output_dir, self.settings.mode)
        logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )

        results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_name, self.settings.models_timeout, now)
        quailty = chain_lib.quality(*results)
        logger.info("results: {} quailty: {} for {}".format(results, quailty, point))
        chain_lib.save_quality(self.output_dir, param_name, quailty)

        self.prev_timestamp = now
        return quailty

    def random_point(self):
        selected_vals = {}
        # return tuple([chain_lib.uniform_int(l, u) for (l, u) in self.data])
        for name in self.info.ordering:
            v=self.param_info[name].random_value(selected_vals)
            selected_vals[name] = v
        return [  selected_vals[name] for name in self.info.ordering ]


    # FIXME
    def do_radius_as_percentage(self, options):
        if options['radius_as_percentage']:
            self.shape = ncuboid
            for s in ['select_radius', 'influence_radius']:
                if s in options:
                    per = options[s]
                    radii = [ math.ceil((u - l) * (per / 100)) for (l, u) in self.data ]
                    options[s] = radii
        else:
            self.shape = ncube

        return options


