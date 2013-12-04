import random


def pick_inside(radii, centre):
	"""Pick a point from a ncuboid of with specified radii"""
	return [random.randint(c - r, c + r) for (r, c) in zip(radii, centre) ]


def is_in_inside(radii, centre, point):
	""" True if point is in a ncuboid of specified radii """
	return not any( (p < c - r or p > c + r) for (p, c, r) in zip(point, centre, radii) )
