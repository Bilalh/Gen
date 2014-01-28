#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   run_params (iterations|time|cpu) <limit> <params>...
   ( --essence=<file> --models_timeout=<int>  --info=<file>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str>]
   run_params json <file> <params>...

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
from pathlib import Path

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir", "limit", "params"])


class RunParams(method.Method):
    def __init__(self, options, limiter, info):
        super(RunParams, self,).__init__(options, limiter, Settings, info)
        self.iter_params = iter(self.settings.params)

    def do_iteration(self):
        param_path = Path(next(self.iter_params))
        with param_path.open() as f:
            param_string = f.read()

        # could refactor to just copy the file, but the file is small, so not worth it
        self.run_param_and_store_quality(param_string, param_path.stem)


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    RunParams(options, limiter, info).run()
    logger.info("<finished>")

