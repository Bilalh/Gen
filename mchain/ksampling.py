#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   ksample (iterations|time|cpu) <limit>
   ( --num_points=<int>  --influence_radius=<int> --essence=<file> --models_timeout=<int> --point_selector=<first!halving!halving_3_4> --info=<file>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage=<bool>]
   ksample json <file>

`time <limit>` is the total time the program can take.
`json` allows reloading of the state including the seed.

Options:
  --help                                        Show this screen.
  --influence_radius=<int>                      Radius for the acceptance function.
  --mode=<str>                                  Conjure mode used [default: df].
  --models_timeout=<int>                        Timeout in seconds.
  --num_points=<int>                            Number of points to pick each time.
  --output_dir=<dir>                            Where to put the results.
  --radius_as_percentage=<bool>                 Radius setting as a % [default: false].
  --seed=<int>                                  Random seed to use.
  --working_dir=<dir>                           Where the essence file is [default: .]
  --point_selector=<first!halving!halving_3_4>  Method to pick the next point
  --info=<file>                    Files that contains the ordering of the variable
"""

from lib import chain_lib
from lib import method
from lib import option_handing

from lib import domains, instances

from collections import namedtuple
from fractions import Fraction
from pprint import pprint, pformat
import itertools
import logging
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['num_points', 'seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir", "limit", "influence_radius", "radius_as_percentage"])


class KSample(method.Method):
    def __init__(self, options, limiter, info):
        super(KSample, self,).__init__(options, limiter, Settings, info)


    def before_settings(self, options):
        point_selectors = {
            "halving_3_4": self.pick_by_halving_3_4,
            "halving": self.pick_by_halving,
            "first":   self.pick_first
        }

        self.point_selector = point_selectors[options['point_selector']]
        del options['point_selector']

        return self.do_radius_as_percentage(options)


    def do_iteration(self):
        picked = self.pick_point()
        logger.info("picked %s", picked)
        self.create_run_param_and_store_quality(picked)
        self.data_points.append(picked)


    def find_mins(self, arr):
        smallest = min(arr)
        return [ e for e in arr if e[0] == smallest[0] ]


    def pick_point(self):

        def rnd_point():
            try:
                return self.random_point()
            except (domains.NoValuesInDomainException, instances.FailedToGenerateParamExeception):
                raise None

        random_points = [ p for p in (rnd_point() for i in range(self.settings.num_points)) if p ]
        assert len(random_points) > 0

        # If we have no data then pick a random point
        if len(self.data_points) == 0:
            return random.choice(random_points)

        def get_quailty(point):
            name = "-".join( [ ("%s" % p.safe) for p in point ] )
            name_hash = chain_lib.hash(name)
            return chain_lib.get_quailty(self.output_dir, name_hash)

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
        logger.info(pformat([ (q, self.point_pretty(x)) for (q, x) in with_quailty ]) )

        chosen = self.pick_next_point(with_quailty)
        return chosen

    def pick_next_point(self, points_in):

        points = sorted(points_in,  key=lambda pt: pt[0] )
        # points = sorted([ (random.randint(0, 7), random.randint(0, 10)) for i in range(10) ])
        # points = [ (i, random.randint(0, 10)) for i in range(10) ]

        def f(k, v):
            l = list(v)
            return (len(l), l)

        grouped = [ f(k, v) for (k, v) in itertools.groupby(points, lambda x: x[0])  ]
        # logger.info(grouped)

        probs = self.point_selector(grouped)

        logger.info("probs %s", probs)
        logger.info("probs sum %s", (sum(probs),  float(sum(probs)) ))

        partial_sums = list(itertools.accumulate(probs))
        logger.info("partial_sums: %s", partial_sums)

        u = random.uniform(0, 1)
        logger.info("u %f", u)

        index = len(probs) -1
        for (i, v) in enumerate(partial_sums):
            if v >= u:
                index = i
                break

        logger.info("picked group %d", index)

        logger.debug(grouped[index])
        choices = list(zip(*grouped[index][1]))[1]
        logger.info(pformat([ self.point_pretty(x) for x in choices ]) )
        return random.choice(choices)



    def pick_first(self, grouped):
        """ Always pick the first from the first group """
        probs = [ 0 for i in range(len(grouped)) ]
        probs[0] = 1
        return probs


    def pick_by_halving(self, grouped):
        """
        1/2        for 1st point
        1/4        for 2nd point
        1/2**(n)   for nth point
        """
        probs = [ 0 for i in range(len(grouped)) ]

        cur = 2
        for (i, (n, _) ) in enumerate(grouped):
            pa = [  Fraction(1, cur * 2 ** j) for j in range(0, n) ]
            # logger.info(pa)

            probs[i] = sum(pa)
            cur = pa[-1].denominator * 2

        return probs

    def pick_by_halving_3_4(self, grouped):
        """
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
        probs = [ 0 for i in range(len(grouped)) ]
        probs[0] = Fraction(3, 4)

        cur = 8
        for (i, (n, _) ) in enumerate(grouped):
            if i == 0 and n ==1:
                continue
            elif i == 0:
                n -= 1

            pa = [  Fraction(1, cur * 2 ** j) for j in range(0, n) ]
            # logger.info(pa)

            probs[i] += sum(pa)
            cur = pa[-1].denominator * 2

        return probs

if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    KSample(options, limiter, info).run()
    logger.info("<finished>")


