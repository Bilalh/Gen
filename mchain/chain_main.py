#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain


"""

Usage:
   chain (iterations|time) <limit>
   ( --chain_length=<int>  --select_radius=<int>  --influence_radius=<int> --essence=<file> --model_timeout=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage]
   chain json <file>


`time <limit>` is the total time the program can take


Options:
  --help                    Show this screen.
  --chain_length=<int>      Length of each chain.
  --select_radius=<int>     Radius for picking next point.
  --influence_radius=<int>  Radius for the acceptance function.
  --model_timeout=<int>     Timeout in seconds.
  --working_dir=<dir>       Where the essence file is [default: .]
  --seed=<int>              Random seed to use.
  --output_dir=<dir>        Where to put the results.
  --essence=<file>          Essence file.
  --radius_as_percentage    Radius setting as in %.
  --mode=<str>              Conjure mode used [default: df].


"""
import option_handing

import logging
logger = logging.getLogger(__name__)

from chain_sampling import Chain

if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)

    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")

    k = Chain(options, limiter)
    k.run()
    logger.info("<finished>")

