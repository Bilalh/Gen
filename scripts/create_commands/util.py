import math


def calc_models_timeout(common, cores):
	return math.ceil(common['total_time'] / (common['races'] + 1) / cores)


def calc_total_time(common, cores):
	return math.ceil(common['total_time'] / cores)