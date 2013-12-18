#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

import random
import pprint
import math

import chain_lib
import ncube
import ncuboid

import os
import sys
import calendar
import json

import logging
logger = logging.getLogger(__name__)


class Chain(object):

    def __init__(self, options, limiter):

        if "PARAM_GEN_SCRIPTS" not in os.environ:
            logger.error("$PARAM_GEN_SCRIPTS needs to defined")
            exit(2)

        if "NUM_JOBS" not in os.environ:
            logger.error("$NUM_JOBS needs to defined")
            exit(3)

        if options['output_dir']:
            self.output_dir = options['output_dir']
        else:
            self.output_dir = os.path.join(options['working_dir'], "out")

        os.makedirs(self.output_dir, exist_ok=True)

        for fp in ["chain", "params"]:
            os.makedirs(os.path.join(self.output_dir, fp), exist_ok=True)

        vals = chain_lib.gather_param_info(options['essence'], self.output_dir)
        logger.info(vals)

        if options['radius_as_percentage']:
            self.shape = ncuboid
            for s in ['select_radius', 'influence_radius']:
                per = options[s]
                radii = [ math.ceil((u - l) * (per / 100)) for (_, (l, u) ) in vals ]
                options[s] = radii

        else:
            self.shape = ncube

        settings = chain_lib.Settings(**options)
        logger.info(settings)

        [self.names, self.data] = list(zip(*vals))
        self.data_points= []
        self._current_iteration = 0
        self.settings = settings

        self.dim = len(self.data)

        if not settings.seed:
            seed = random.randint(0, 2 ** 32)
        else:
            seed = settings.seed

        self.limiter = limiter

        logger.info("Using Seed {}".format(seed))
        random.seed(seed)



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


    def first_point(self, data):
        return tuple([random.randint(l, u) for (l, u) in data])


    def next_point(self, current_chain):
        return [int(x) for x in self.shape.pick_inside(self.settings.select_radius, current_chain[-1])]


    def make_chain(self):
        current_chain = [self.first_point(self.data)]
        for i in range(self.settings.chain_length):
            candidate_point = self.next_point(current_chain)
            if self.acceptance(current_chain[-1], candidate_point, self.data, None):
                current_chain.append(candidate_point)

        logger.debug("Chain:")
        logger.debug([ [int(v) for v in vs] for vs in current_chain])

        with open(os.path.join(self.output_dir, "chain", "iter-{:03}-chain.json".format(self._current_iteration)), "w") as f:
            f.write(json.dumps(current_chain))

        self._current_iteration+=1
        return current_chain[-1]


    def run(self):
        self.limiter.start()
        while self.limiter.continue_running():
            selected_point = self.make_chain()
            self.data_points.append(selected_point)

            (param_string, param_name) = chain_lib.create_param_essence(zip(self.names, self.data_points[-1]))
            logger.info(param_string)
            param_path = chain_lib.write_param(self.output_dir, param_string, param_name)

            datee = calendar.datetime.datetime.now()
            logger.info("Start %s", datee.isoformat())
            now = str(int(datee.timestamp()))

            chain_lib.run_models(now, param_path, self.settings.model_timeout, self.settings.working_dir, self.output_dir, mode="df")
            logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )

            results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_name, self.settings.model_timeout, now)
            quailty = chain_lib.quality(*results)
            logger.info("results: {} quailty: {}".format(results, quailty))
            chain_lib.save_quality(self.output_dir, param_name, quailty)


        with open(os.path.join(self.output_dir, "chain", "data-points.json"), "w") as f:
            f.write(json.dumps(self.data_points))


if __name__ == "__main__":
    logging.basicConfig(format='%(name)s:%(levelname)s: %(message)s', level=logging.info)
    from limit import IterationsLimit
    s = chain_lib.Settings(select_radius=10, influence_radius=10, chain_length=20,
        model_timeout=80, seed=None, output_dir=None, limit=2,
        essence="/Users/bilalh/CS/instancegen-models/prob024-Langford/prob024-Langford.essence",
        working_dir="/Users/bilalh/CS/instancegen-models/prob024-Langford", mode='df')
    chain = Chain(s, IterationsLimit(s.limit))
    chain.run()




