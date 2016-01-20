#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   uniform (iterations|time|cpu) <limit>
   ( --essence=<file> --models_timeout=<int>  --info=<file>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --use_minion=<bool> --pre_generate=<bool> --generated_dir=<dir>   --timeout=<simple!dynamic!exponential> ]
   uniform json <file>

`time <limit>` is the total time the program can take.
`json` allows reloading of the state including the seed.

Options:
  --help                    Show this screen.
  --essence=<file>          Essence file.
  --mode=<str>              Conjure mode used [default: df].
  --models_timeout=<int>    Timeout in seconds.
  --output_dir=<dir>        Where to put the results.
  --seed=<int>              Random seed to use.
  --working_dir=<dir>       Where the essence file is [default: .]
  --info=<file>             Files that contains the ordering of the variables
  --use_minion=<bool>       Uses Minion to generate params [default: true]
  --pre_generate=<bool>     When using minion, genrate all solution once and pick from them [default: false]
  --generated_dir=<dir>     Directory to place all solutions, specs, which can be reused between runs
  --timeout=<simple!dynamic!exponential>       Timeout method to use [default: simple]

"""

from lib import chain_lib
from lib import option_handing
from lib import method
from lib import domains, instances

from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir",
                                "output_dir", "limit", "use_minion", "pre_generate", "generated_dir" ])


class UniformSampling(method.Method):
    def __init__(self, options, limiter, info):
        super(UniformSampling, self,).__init__(options, limiter, Settings, info)

    def before_settings(self, options):
        return self.do_timeout_way(options)

    def do_iteration(self):
        picked = self.random_point()
        logger.info("Picked %s", picked)
        self.create_run_param_and_store_quality(picked)
        self.data_points.append(picked)


if __name__ == '__main__':
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    chain_lib.setup_logging( options )
    UniformSampling(options, limiter, info).run()
    logger.info("<finished>")

