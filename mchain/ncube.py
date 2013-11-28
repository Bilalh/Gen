import random


def pick_inside_ncube(radius, centre):
	"""Pick a point from a ncube of with specified radius"""
	return [random.randint(c - radius, c + radius) for c in centre]


def is_in_ncube(radius, centre, point):
	""" True if point is in a ncube of specified radius """
	return not any( (p < c - radius or p > c + radius) for (p, c) in zip(point, centre) )