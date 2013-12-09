#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   nsample (iterations|time) <limit>
   ( --essence=<file> --model_timeout=<int> --influence_radius=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage]

`time <limit>` is the total time the program can take

Options:
  --help                    Show this screen.
  --influence_radius=<int>  Radius for the acceptance function.
  --model_timeout=<int>     Timeout in seconds.
  --working_dir=<dir>       Where the essence file is [default: .]
  --seed=<int>              Random seed to use.
  --output_dir=<dir>        Where to put the results.
  --essence=<file>          Essence file.
  --mode=<str>              Conjure mode used [default: df].
  --radius_as_percentage    Radius setting as in %.

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
Settings=namedtuple('Settings', ['seed', 'mode', 'model_timeout', "essence", "working_dir", "output_dir", "limit", "influence_radius", "radius_as_percentage"])


class NSample(method.Method):
    def __init__(self, options, limiter):
        super(NSample, self,).__init__(options, limiter, Settings)
        self.goodness_x_prev = 1

    def before_settings(self, options):
        if options['radius_as_percentage']:
            self.shape = ncuboid
            per = options['influence_radius']
            radii = [ math.ceil((u - l) * (per / 100)) for (l, u) in self.data ]
            options['influence_radius'] = radii
        else:
            self.shape = ncube

        return options

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
    k = NSample(options, limiter)
    k.run()
    logger.info("<finished>")

