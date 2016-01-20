#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   sampling (cpu) <limit>
   ( --essence=<file> --models_timeout=<int>  --info=<file>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --use_minion=<bool> --timeout=<simple!dynamic!exponential> --pre_generate=<bool>  --generated_dir=<dir> ]
   sampling json <file>

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
  --timeout=<simple!dynamic!exponential>       Timeout method to use [default: simple]
  --pre_generate=<bool>      When using minion, genrate all solution once and pick from them [default: false]
  --generated_dir=<dir>      Directory to place all solutions, specs, which can be reused between runs
"""

from lib import option_handing
from lib import method

from collections import namedtuple
import logging
from pathlib import Path

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir",
                                 "limit", "use_minion", "pre_generate", "generated_dir"])


class Sampling(method.Method):
    def __init__(self, options, limiter, info):
        super(Sampling, self,).__init__(options, limiter, Settings, info)

        self.genrate_compact()

    def before_settings(self, options):
        return self.do_timeout_way(options)


    def stage1():
        picked = self.random_point()
        logger.info("Picked %s", picked)

        # Run instances

        self.data_points.append(picked)

        if timed_out:
            self.stage1_done |= False
        else:
            self.stage1_done = True


    def do_iteration(self):

        if not self.stage1_done:
            self.stage1()


        picked = self.random_point()
        logger.info("Picked %s", picked)


        raise NotImplementedError("")
        self.data_points.append(picked)



if __name__ == '__main__':
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    Sampling(options, limiter, info).run()
    logger.info("<finished>")
