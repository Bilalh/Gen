#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   nsample (iterations|time|cpu) <limit>
   ( --essence=<file> --models_timeout=<int> --influence_radius=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage=<bool>]
   nsample json <file>

`time <limit>` is the total time the program can take.
`json` allows reloading of the state including the seed.

Options:
  --help                           Show this screen.
  --influence_radius=<int>         Radius for the acceptance function.
  --essence=<file>                 Essence file.
  --mode=<str>                     Conjure mode used [default: df].
  --models_timeout=<int>           Timeout in seconds.
  --output_dir=<dir>               Where to put the results.
  --radius_as_percentage=<bool>    Radius setting as a % [default: false ].
  --seed=<int>                     Random seed to use.
  --working_dir=<dir>              Where the essence file is [default: .]

"""

from lib import chain_lib
from lib import method
from lib import option_handing

from collections import namedtuple
import logging
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir", "limit", "influence_radius", "radius_as_percentage"])


class NSample(method.Method):
    def __init__(self, options, limiter):
        super(NSample, self,).__init__(options, limiter, Settings)
        self.goodness_x_prev = 1


    def before_settings(self, options):
        return self.do_radius_as_percentage(options)


    def goodness(self, point):

        def get_quailty(x):
            name = "-".join( [ ("%03d" % p) for p in x ] )
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

        quailty = avg_quality(point)
        return 1 - quailty


    def do_iteration(self):
        x = self.random_point()
        goodness_x = self.goodness(x)
        logger.info("point %s,  goodness_x: %0.3f goodness_x_prev: %0.3f", x, goodness_x, self.goodness_x_prev)

        def accept_point():
            #  if goodness_x and goodness_x_prev are both zero should we reject it?
            if self.goodness_x_prev == 0:
                logger.info("Unconditionally accepting since goodness_x_prev is 0")
                return True

            accept = goodness_x / self.goodness_x_prev
            if accept >= 1:
                logger.info("Unconditionally accepting %0.3f", accept)
                return True
            else:
                u = random.uniform(0, 1)
                logger.info("accept:%0.3f,  u:%0.3f, %s ", accept, u, u < accept)
                return u < accept

        if accept_point():
            self.run_param_and_store_quality(x)
            self.data_points.append(x)

        self.goodness_x_prev = goodness_x


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")
    NSample(options, limiter).run()
    logger.info("<finished>")

