#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   uniform (iterations|time|cpu) <limit>
   ( --essence=<file> --models_timeout=<int>  --info=<file>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str>]
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
"""

from lib import option_handing
from lib import method

from collections import namedtuple
import logging

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir", "limit"])


class UniformSampling(method.Method):
    def __init__(self, options, limiter, info):
        super(UniformSampling, self,).__init__(options, limiter, Settings, info)

    def do_iteration(self):
        picked = self.random_point()
        # picked = (4, 1)
        logger.info("Picked %s", picked)
        self.run_param_and_store_quality(picked)


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    UniformSampling(options, limiter, info).run()
    logger.info("<finished>")

