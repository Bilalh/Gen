#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain


"""

Usage:
   chain (time|iterations) <limit>
   ( --chain_length=<int>  --select_radius=<int>  --influence_radius=<int> --essence=<file> )
   [ --model_timeout=<int> --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str>]

Options:
  --help                    Show this screen.
  --chain_length=<int>      Length of each chain.
  --select_radius=<int>     Radius for picking next point.
  --influence_radius=<int>  Radius for the acceptance function.
  --model_timeout=<int>     Timeout in seconds [default: 600].
  --working_dir=<dir>       Where the essence file is [default: .]
  --seed=<int>              Random seed to use
  --output_dir=<dir>        Where to put the results
  --essence=<file>          Essence file
  --mode<str>               Conjure mode used [default: df]

"""
from docopt import docopt
import re
import os

from chain_lib import Settings
from chain_sampling import Chain
import limit

if __name__ == '__main__':

    limiters = {
        "time": limit.TimeLimit,
        "iterations": limit.IterationsLimit
    }

    arguments = docopt(__doc__, version='')

    for l in limiters:
        if arguments[l]:
            limiter_s = limiters[l]
        del arguments[l]

    print(arguments)
    print(limiter_s)

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
    print(options)

    options['limit'] = int(options['limit'])
    settings = Settings(**options)
    print(settings)

    limiter = limiter_s(settings.limit)

    chain = Chain(settings, limiter)
    chain.run()
