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

import operator


class KSample(object):

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

		for fp in ["info", "params"]:
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
		self.settings = settings

		[self.names, self.data] = list(zip(*vals))

		self.dim = len(self.data)
		self._current_iteration = 0

		self.limiter = limiter

		if not settings.seed:
			seed = random.randint(0, 2 ** 32)
		else:
			seed = settings.seed


		logger.info("Using Seed {}".format(seed))
		random.seed(seed)

		# To shown it works
		self.data_points = []
		# self.data_points = [(1, 3), (1, 7), (1, 8), (1, 9), (1, 10), (1, 12), (1, 13), (1, 15), (1, 16), (1, 17), (1, 18), (1, 20), (1, 23), (1, 24), (1, 25), (1, 26), (1, 28), (1, 29), (2, 3), (2, 4), (2, 5), (2, 6), (2, 7), (2, 8), (2, 11), (3, 3), (3, 4), (3, 5), (3, 6), (3, 7), (3, 8), (3, 9), (3, 11), (4, 1), (4, 3), (4, 4), (4, 5), (4, 6), (4, 7), (4, 8), (4, 9), (4, 10), (5, 1), (5, 2), (5, 4), (5, 5), (5, 6), (5, 7), (5, 8)]


	def random_point(self):
		return tuple([random.randint(l, u) for (l, u) in self.data])


	def do_iteration(self):
		random_points = [ self.random_point() for i in range(self.settings.chain_length) ]

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
			logger.info("rp (%0.2f,%s)", v, p)

		mins_with_quailty = self.find_mins(with_quailty)

		mins = list(zip(*mins_with_quailty))[1]
		logger.info("mins @ %f %s ", mins_with_quailty[0][0], mins)

		# If multiple minimums pick randomly from them
		chosen = random.choice(mins)
		return chosen


	def find_mins(self, arr):
		smallest = min(arr)
		return [ e for e in arr if e[1] == smallest[1] ]


	def run(self):
		self.limiter.start()
		while self.limiter.continue_running():
			selected_point = self.do_iteration()
			logger.info("Picked %s", selected_point)

			self.data_points.append(selected_point)

			(param_string, param_name) = chain_lib.create_param_essence(zip(self.names, self.data_points[-1]))
			logger.info(param_string)
			param_path = chain_lib.write_param(self.output_dir, param_string, param_name)

			datee = calendar.datetime.datetime.now()
			logger.info("Start %s", datee.isoformat())
			now = str(int(datee.timestamp()))

			chain_lib.run_models(now, param_path, self.settings.model_timeout, self.settings.working_dir, self.output_dir)
			logger.info("End %s", calendar.datetime.datetime.now().isoformat()  )

			results = chain_lib.get_results(self.settings.working_dir, self.output_dir, param_name, self.settings.model_timeout, now)
			quailty = chain_lib.quality(*results)
			logger.info("point {} results: {} quailty: {}".format(selected_point, results, quailty))
			chain_lib.save_quality(self.output_dir, param_name, quailty)

