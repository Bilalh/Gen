#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

"""
Usage:
   chain (iterations|time|cpu) <limit>
   ( --chain_length=<int>  --select_radius=<int>  --influence_radius=<int> --essence=<file> --models_timeout=<int>)
   [ --working_dir=<dir> --seed=<int> --output_dir=<dir> --mode=<str> --radius_as_percentage=<bool>]
   chain json <file>

`time <limit>` is the total time,  cpu <limit> is the total cputime.
`json` allows reloading of the state including the seed.

Options:
  --help                           Show this screen.
  --chain_length=<int>             Length of each chain.
  --essence=<file>                 Essence file.
  --influence_radius=<int>         Radius for the acceptance function.
  --mode=<str>                     Conjure mode used [default: df].
  --models_timeout=<int>           Timeout in seconds.
  --output_dir=<dir>               Where to put the results.
  --radius_as_percentage=<bool>    Radius setting as a % [default: false].
  --seed=<int>                     Random seed to use.
  --select_radius=<int>            Radius for picking next point.
  --working_dir=<dir>              Where the essence file is [default: .]
"""

from lib import chain_lib
from lib import method
from lib import option_handing

from collections import namedtuple
import json
import logging
import os
import random

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['chain_length', 'select_radius', 'influence_radius', 'seed', 'mode',
                                'models_timeout', "essence", "working_dir", "output_dir", "limit", "radius_as_percentage"])


class Chain(method.Method):
    def __init__(self, options, limiter):
        super(Chain, self,).__init__(options, limiter, Settings)

        for fp in ["chain"]:
            os.makedirs(os.path.join(self.output_dir, fp), exist_ok=True)

        self.dim = len(self.data)


    def before_settings(self, options):
        return self.do_radius_as_percentage(options)


    def acceptance(self, previous_point, candidate_point, data, local_data):
        """ Return True if the  candidate_point should be added to the chain """

        logger.debug(("acceptance", previous_point, candidate_point, data, local_data))
        # Bound check
        if previous_point == candidate_point or any( (p < l or p > u) for (p, (l, u)) in zip(candidate_point, data)):
            logger.debug(("rejected", candidate_point))
            return False

        # Get points that effect
        points = [ p for p in self.data_points if self.shape.is_in_inside(self.settings.influence_radius, candidate_point, p) ]
        logger.debug("influence points for {} are : {}".format(candidate_point, points))

        if len(points) == 0:
            return random.choice([True, False])

        def get_quailty(point):
            name = "-".join( [ ("%03d" % p) for p in point ] )
            return chain_lib.get_quailty(self.output_dir, name)

        # Get the quailties of these points
        quailties = [get_quailty(p) for p in points]
        logger.debug("quailties {}".format(quailties))

        mean = sum(quailties) / len(quailties)
        choice = random.uniform(0, 1)
        logger.debug("mean is {}, choice is {}".format(mean, choice))

        return (choice >= mean)


    def make_chain(self):
        def first_point(data):
            return self.random_point()

        current_chain = [first_point(self.data)]
        for i in range(self.settings.chain_length):
            candidate_point = self.next_point(current_chain)
            if self.acceptance(current_chain[-1], candidate_point, self.data, None):
                current_chain.append(candidate_point)

        logger.debug("Chain:")
        logger.debug([ [int(v) for v in vs] for vs in current_chain])

        with open(os.path.join(self.output_dir, "chain", "iter-{:03}-chain.json".format(self._current_iteration)), "w") as f:
            f.write(json.dumps(current_chain))

        return current_chain[-1]


    def next_point(self, current_chain):
        return [int(x) for x in self.shape.pick_inside(self.settings.select_radius, current_chain[-1])]


    def do_iteration(self):
            selected_point = self.make_chain()
            self.data_points.append(selected_point)
            self.create_run_param_and_store_quality(selected_point)


if __name__ == '__main__':
    logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.INFO)
    (options, limiter) = option_handing.parse_arguments(__doc__, version="1.0")
    Chain(options, limiter).run()
    logger.info("<finished>")


