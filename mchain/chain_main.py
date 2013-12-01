#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain


"""

Usage:
   chain (iterations|time) <limit>
   ( --chain_length=<int>  --select_radius=<int>  --influence_radius=<int> --essence=<file> --model_timeout=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage]

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
from docopt import docopt
import re
import os

from chain_sampling import Chain
import limit
import pprint

import logging
logger = logging.getLogger(__name__)

if __name__ == '__main__':
    logging.basicConfig(format='%(name)s: %(message)s', level=logging.INFO)
    # logging.basicConfig(format='%(name)s:%(levelname)s: %(message)s', level=logging.INFO)

    limiters = {
        "time": limit.TimeLimit,
        "iterations": limit.IterationsLimit
    }


    arguments = docopt(__doc__, version='1.0')

    for l in limiters:
        if arguments[l]:
            limiter_s = limiters[l]
        del arguments[l]

    logger.debug(arguments)
    logger.debug(limiter_s)

    # Convert ints to ints
    for key in re.findall(r"(--[_a-zA-Z]+)=<int>", __doc__):
        if arguments[key]:
            arguments[key] = int(arguments[key])

    # Convert dirs to abspath
    for (key, _) in re.findall(r"(--[_a-zA-Z]+)=<(dir|file)>", __doc__):
        if arguments[key]:
            arguments[key] = os.path.abspath(os.path.expanduser(arguments[key]))

    # remove -- from the start and <> around positional arguments
    options = { k.strip('<>').replace('--', ''): v for (k, v) in arguments.items()  }
    logger.info(pprint.pformat(options))

    options['limit'] = int(options['limit'])

    limiter = limiter_s(options['limit'])
    chain = Chain(options, limiter)
    chain.run()
