import math


def calc_model_timeout(common, cores):
	return math.ceil(math.ceil(common['total_time'] / common['races']) / cores)


def calc_total_time(common, cores):
	return math.ceil(common['total_time'] / cores)