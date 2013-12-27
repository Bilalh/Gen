#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   ksample (iterations|time|cpu) <limit>
   ( --num_points=<int>  --influence_radius=<int> --essence=<file> --models_timeout=<int> --point_selector=<first|halves> )
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage=<bool>]
   ksample json <file>

`time <limit>` is the total time the program can take.
`json` allows reloading of the state including the seed.

Options:
  --help                            Show this screen.
  --influence_radius=<int>          Radius for the acceptance function.
  --mode=<str>                      Conjure mode used [default: df].
  --models_timeout=<int>            Timeout in seconds.
  --num_points=<int>                Number of points to pick each time.
  --output_dir=<dir>                Where to put the results.
  --radius_as_percentage=<bool>     Radius setting as a % [default: false].
  --seed=<int>                      Random seed to use.
  --working_dir=<dir>               Where the essence file is [default: .]
  --point_selector=<first|halves>   Method to pick the next point
"""

from lib import chain_lib
from lib import method
from lib import option_handing

from collections import namedtuple
from fractions import Fraction
from pprint import pprint
import itertools
import logging
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['num_points', 'seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir", "limit", "influence_radius", "radius_as_percentage"])


class KSample(method.Method):
    def __init__(self, options, limiter):
        super(KSample, self,).__init__(options, limiter, Settings)
        # To shown it works
        # self.data_points = [(1, 3), (1, 7), (1, 8), (1, 9), (1, 10), (1, 12), (1, 13), (1, 15), (1, 16), (1, 17), (1, 18), (1, 20), (1, 23), (1, 24), (1, 25), (1, 26), (1, 28), (1, 29), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 11), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9), (3, 11), (4, 1), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (4, 10), (5, 1), (5, 2), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8)]


    def before_settings(self, options):
        point_selectors = {
            "halves": self.pick_next_point
        }

        self.point_selector = point_selectors[options['point_selector']]
        del options['point_selector']

        return self.do_radius_as_percentage(options)


    def do_iteration(self):
        picked = self.pick_point()
        logger.info("picked %s", picked)
        self.run_param_and_store_quality(picked)
        self.data_points.append(picked)


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

        chosen = self.pick_next_point(with_quailty)
        return chosen


    def pick_next_point(self, points_in):
        """
        Change of picking each point
        3/4        for 1st point
        1/8        for 2nd point
        1/2**(n+1) for nth point

        ties
        3/4    [(1, [(0, 5)]),
        7/32   (3,  [(1, 3), (1, 8), (1, 8)]),
        7/256  (3,  [(2, 0), (2, 4), (2, 10)]),
        1/512  (1,  [(5, 5)]),
        3/2048 (2,   [(6, 2), (6, 8)])]
        """
        points = sorted(points_in)
        # points = sorted([ (random.randint(0, 7), random.randint(0, 10)) for i in range(10) ])


        def f(k, v):
            l = list(v)
            return (len(l), l)

        grouped = [ f(k, v) for (k, v) in itertools.groupby(points, lambda x: x[0])  ]

        probs = [ 0 for i in range(len(grouped)) ]

        cur = 2
        for (i, (n, _) ) in enumerate(grouped):
            pa = [  Fraction(1, cur * 2 ** i) for i in range(0, n  ) ]
            # logger.info(pa)

            probs[i] = sum(pa)
            cur = pa[-1].denominator * 2


        logger.info("probs %s", probs)
        # logger.info((sum(probs),  float(sum(probs)) ))

        partial_sums = itertools.accumulate(probs)
        u = random.uniform(0, 1)
        logger.info(u)

        index = len(probs) -1
        for (i, v) in enumerate(partial_sums):
            if v > u:
                index = i
                break

        logger.debug(grouped[index])
        choices = list(zip(*grouped[index][1]))[1]
        logger.info(choices)

        return random.choice(choices)


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")
    KSample(options, limiter).run()
    logger.info("<finished>")

