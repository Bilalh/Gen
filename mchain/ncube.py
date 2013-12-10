import chain_lib


def pick_inside(radius, centre):
	"""Pick a point from a ncube of with specified radius"""
	return [chain_lib.uniform_int(c - radius, c + radius) for c in centre]


def is_in_inside(radius, centre, point):
	""" True if point is in a ncube of specified radius """
	return not any( (p < c - radius or p > c + radius) for (p, c) in zip(point, centre) )