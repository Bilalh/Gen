#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   nsample (iterations|time|cpu) <limit>
   ( --essence=<file> --models_timeout=<int> --influence_radius=<int> --info=<file> )
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage=<bool> --use_minion=<bool> --pre_generate=<bool>  --generated_dir=<dir>]
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
  --radius_as_percentage=<bool>    Radius setting as a % [default: false].
  --seed=<int>                     Random seed to use.
  --working_dir=<dir>              Where the essence file is [default: .]
  --info=<file>                    Files that contains the ordering of the variable
  --use_minion=<bool>              Uses Minion to generate params [default: true]
  --pre_generate=<bool>            When using minion, genrate all solution once and pick from them [default: false]
  --generated_dir=<dir>            Directory to place all solutions, specs, which can be reused between runs


"""

from lib import domains
from lib import instances
from lib import chain_lib
from lib import method
from lib import option_handing

from collections import namedtuple
import logging
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['seed', 'mode', 'models_timeout', "essence", "working_dir", "output_dir",
                                 "limit", "influence_radius", "radius_as_percentage", "use_minion",
                                 "generated_dir", "pre_generate"])


class NSample(method.Method):
    def __init__(self, options, limiter, info):
        super(NSample, self,).__init__(options, limiter, Settings, info)
        self.rejected_series=0

    def before_settings(self, options):
        return self.do_radius_as_percentage(options)

    def goodness(self, point):

        def avg_quality(rp):
            # TODO can made more efficient
            influence_points = [ p for p in self.data_points
                if self.shape.is_in_inside(self.settings.influence_radius, rp, p) ]

            if (len(influence_points) == 0):
                logger.info("no influence_points")
                return 0.5

            logger.info("len(influence_points) %s len(data_points) %s ",
                len(influence_points), len(self.data_points))

            quailties = [self.get_quailty(p) for p in influence_points]
            logger.info(quailties)

            mean = sum(quailties) / len(quailties)
            return mean

        # Should base the the influence based on how far the point is
        quailty = avg_quality(point)
        return 1 - quailty


    def do_iteration(self):
        x = self.random_point()

        logger.info("made point x %s", x)
        logger.info("X %s", self.point_pretty(x))

        goodness_x = self.goodness(x)

        if len(self.data_points) > 1:
            goodness_x_prev = 1 - self.get_quailty(self.data_points[-1])
            logger.info("Using previous data point")
        else:
            goodness_x_prev = 1
            logger.info("No previous data point")

        logger.info("point: goodness_x: %0.3f goodness_x_prev: %0.3f pretty %s", goodness_x, goodness_x_prev, [y.pretty for y in x ])


        def accept_point():
            if goodness_x_prev == 0:
                logger.info("Unconditionally accepting since goodness_x_prev is 0")
                return True

            accept = goodness_x / goodness_x_prev
            if accept >= 1:
                logger.info("Unconditionally accepting %0.3f", accept)
                return True
            elif self.rejected_series > 100:
                # To account the radius being too large and influence points saying everything is useless
                return random.choice([True,False])
            else:
                u = random.uniform(0, 1)
                logger.info("accept:%0.3f,  u:%0.3f, %s ", accept, u, u <= accept)
                # To account for case accept=0
                return u <= (accept + 1e-5)

        if accept_point():
            self.rejected_series=0
            self.create_run_param_and_store_quality(x)
            self.data_points.append(x)
        else:
            self.rejected_series+=1
            logger.info("REJECTED point %s,  goodness_x: %0.3f goodness_x_prev: %0.3f", x, goodness_x, goodness_x_prev)
            raise domains.DontCountIterationException()


        logger.info("do_iteration end")


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter, info) = option_handing.parse_arguments(__doc__, version="1.0")
    NSample(options, limiter, info).run()
    logger.info("<finished>")

