#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import chain_lib
import math
import method
import ncube
import ncuboid
# import option_handing

from collections import namedtuple
import logging
import random
import os
import json

logger = logging.getLogger(__name__)
Settings=namedtuple('Settings', ['chain_length', 'select_radius', 'influence_radius', 'seed', 'mode',
                                'model_timeout', "essence", "working_dir", "output_dir", "limit", "radius_as_percentage"])


class Chain(method.Method):

    def __init__(self, options, limiter):
        super(Chain, self,).__init__(options, limiter, Settings)

        for fp in ["chain"]:
            os.makedirs(os.path.join(self.output_dir, fp), exist_ok=True)

        self.dim = len(self.data)


    def before_settings(self, options):
        if options['radius_as_percentage']:
            self.shape = ncuboid
            per = options['influence_radius']
            for s in ['select_radius', 'influence_radius']:
                per = options[s]
                radii = [ math.ceil((u - l) * (per / 100)) for (_, (l, u) ) in self.data ]
                options[s] = radii
        else:
            self.shape = ncube

        return options


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
            self.run_param_and_store_quality(selected_point)




