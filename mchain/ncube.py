import random


def pick_inside_ncube(radius, centre):
	"""Pick a point from a ncube of with specified radius"""
	return [random.randint(c - radius, c + radius) for c in centre]


def is_in_ncube(radius, centre, point):
	""" True if point is in a ncube of specified radius """
	return not any( (p < c - radius or p > c + radius) for (p, c) in zip(point, centre) )


def pick_inside_ncube2(radii, centre):
	"""Pick a point from a ncube of with specified radii"""
	return [random.randint(c - radius, c + radius) for (radius, c) in zip(radii, centre) ]
