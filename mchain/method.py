import chain_lib

from abc import ABCMeta, abstractmethod
import calendar
import json
import logging
import math
import os
import random

logger = logging.getLogger(__name__)


class Method(metaclass=ABCMeta):
    def __init__(self, options, limiter, setting_constructor):
        if "PARAM_GEN_SCRIPTS" not in os.environ:
            logger.error("$PARAM_GEN_SCRIPTS needs to defined")
            exit(2)

        if "NUM_JOBS" not in os.environ:
            logger.error("$NUM_JOBS needs to defined")
            exit(3)

        if options['output_dir']:
            self.output_dir = options['output_dir']
        else:
            self.output_dir = os.path.join(options['working_dir'], "out")

        os.makedirs(self.output_dir, exist_ok=True)

        for fp in ["info", "params"]:
            os.makedirs(os.path.join(self.output_dir, fp), exist_ok=True)

        if not options['seed']:
            options['seed'] = random.randint(0, 2 ** 32)

        with open(os.path.join(self.output_dir, "info", "rerun_settings.json"), "w") as f:
            options['limiter'] = limiter.__class__.__name__
            f.write(json.dumps(options))
            del options['limiter']

        vals = chain_lib.gather_param_info(options['essence'], self.output_dir)
        logger.info(vals)

        [self.names, self.data] = list(zip(*vals))
        self.limiter = limiter


        options = self.before_settings(options)
        settings = setting_constructor(**options)
        logger.info(settings)
        self.settings = settings


        logger.info("Using Seed {}".format(settings.seed))
        random.seed(settings.seed)

        self.data_points = []
        self._current_iteration = 0



    def run(self):
        self.limiter.start()
        while self.limiter.continue_running():
            self.do_iteration()
            self._current_iteration+=1

        with open(os.path.join(self.output_dir, "info", "data-points.json"), "w") as f:
            f.write(json.dumps(self.data_points))

    @abstractmethod
    def do_iteration():
        pass

    def before_settings(self, options):
        return options

    def run_param_and_store_quality(self, point):
        (param_string, param_name) = chain_lib.create_param_essence(zip(self.names, point))
        logger.info(param_string)
        param_path = chain_lib.write_param(self.output_dir, param_string, param_name)

        datee = calendar.datetime.datetime.now()
        logger.info("Start %s", datee.isoformat())
        now = str(int(datee.timestamp()))

        chain_lib.run_models(now, param_path, self.settings.model_timeout, self.settings.working_dir, self.output_dir)
        logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )

        results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_name, self.settings.model_timeout, now)
        quailty = chain_lib.quality(*results)
        logger.info("results: {} quailty: {}".format(results, quailty))
        chain_lib.save_quality(self.output_dir, param_name, quailty)

        return quailty

    def random_point(self):
        def uniform_int(l, u):
            return math.ceil(random.uniform(l, u))

        return tuple([uniform_int(l, u) for (l, u) in self.data])

