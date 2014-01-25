from lib import chain_lib
import math


def pick_inside(radius, centre):
	raise NotImplementedError


def is_in_inside(radius, centre, point):
	""" True if the ecuidean distance from the centre is less then the specified radius """
	summed =sum(  (c.distance(p)) ** 2 for (p, c) in zip(point, centre) )
	# might be faster to do summed <= radius ** 2
	return math.sqrt(summed) <= radius
