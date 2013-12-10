#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   ksample (iterations|time) <limit>
   ( --num_points=<int>  --influence_radius=<int> --essence=<file> --model_timeout=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage]
   ksample json <file>

`time <limit>` is the total time the program can take.
`json` allows reloading of the state including the seed.

Options:
  --help                    Show this screen.
  --num_points=<int>        Number of points to pick each time.
  --influence_radius=<int>  Radius for the acceptance function.
  --model_timeout=<int>     Timeout in seconds.
  --working_dir=<dir>       Where the essence file is [default: .]
  --seed=<int>              Random seed to use.
  --output_dir=<dir>        Where to put the results.
  --essence=<file>          Essence file.
  --radius_as_percentage    Radius setting as in %.
  --mode=<str>              Conjure mode used [default: df].
"""

import chain_lib
import math
import method
import ncube
import ncuboid

from collections import namedtuple
import logging
import option_handing
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['num_points', 'seed', 'mode', 'model_timeout', "essence", "working_dir", "output_dir", "limit", "influence_radius", "radius_as_percentage"])


class KSample(method.Method):
    def __init__(self, options, limiter):
        super(KSample, self,).__init__(options, limiter, Settings)
        # To shown it works
        # self.data_points = [(1, 3), (1, 7), (1, 8), (1, 9), (1, 10), (1, 12), (1, 13), (1, 15), (1, 16), (1, 17), (1, 18), (1, 20), (1, 23), (1, 24), (1, 25), (1, 26), (1, 28), (1, 29), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 11), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9), (3, 11), (4, 1), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (4, 10), (5, 1), (5, 2), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8)]


    def before_settings(self, options):
        if options['radius_as_percentage']:
            self.shape = ncuboid
            per = options['influence_radius']
            radii = [ math.ceil((u - l) * (per / 100)) for (l, u) in self.data ]
            options['influence_radius'] = radii
        else:
            self.shape = ncube

        return options


    def do_iteration(self):
        picked = self.pick_point()
        logger.info("picked %s", picked)
        self.run_param_and_store_quality(picked)


    def find_mins(self, arr):
        smallest = min(arr)
        return [ e for e in arr if e[0] == smallest[0] ]


    def pick_point(self):
        random_points = [ self.random_point() for i in range(self.settings.num_points) ]

        # If we have no data then pick a random point
        if len(self.data_points) == 0:
            return random.choice(random_points)

        def get_quailty(point):
            name = "-".join( [ ("%03d" % p) for p in point ] )
            return chain_lib.get_quailty(self.output_dir, name)

        def avg_quality(rp):
            # TODO can made more efficient
            influence_points = [ p for p in self.data_points
                if self.shape.is_in_inside(self.settings.influence_radius, rp, p) ]

            if (len(influence_points) == 0):
                return 0.5

            quailties = [get_quailty(p) for p in influence_points]

            mean = sum(quailties) / len(quailties)
            return mean


        with_quailty = [ (avg_quality(rp), rp) for rp in random_points ]
        for (v, p) in sorted(with_quailty):
            logger.info("rp (%0.4f,%s)", v, p)

        mins_with_quailty = self.find_mins(with_quailty)

        mins = list(zip(*mins_with_quailty))[1]
        logger.info("mins @ %f %s ", mins_with_quailty[0][0], mins)

        # If multiple minimums pick randomly from them
        chosen = random.choice(mins)
        return chosen


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)

    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")
    k = KSample(options, limiter)
    k.run()
    logger.info("<finished>")
