#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   uniform (iterations|time) <limit>
   ( --essence=<file> --model_timeout=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str>]

`time <limit>` is the total time the program can take

Options:
  --help                    Show this screen.
  --model_timeout=<int>     Timeout in seconds.
  --working_dir=<dir>       Where the essence file is [default: .]
  --seed=<int>              Random seed to use.
  --output_dir=<dir>        Where to put the results.
  --essence=<file>          Essence file.
  --mode=<str>              Conjure mode used [default: df].

"""
import option_handing
import logging
import method
from collections import namedtuple

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'model_timeout', "essence", "working_dir", "output_dir", "limit"])


class UniformSampling(method.Method):
    def __init__(self, options, limiter):
        super(UniformSampling, self,).__init__(options, limiter, Settings)

    def do_iteration(self):
        picked = self.random_point()
        logger.info("Picked %s", picked)
        self.run_param_and_store_quality(picked)


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)

    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")
    k = UniformSampling(options, limiter)
    k.run()
    logger.info("<finished>")

