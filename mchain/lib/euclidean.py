import math
import logging
from pprint import pprint, pformat

logger = logging.getLogger(__name__)


def pick_inside(method, radius, centre):

	logger.info("radius %s, centre  %s", pformat(radius), pformat(method.point_pretty(centre))  )

	# [  self.param_info[name]. for (name, p) in zip(method.info.ordering, x) ]

	selected_vals = {}

	for (name, instance) in zip(method.info.ordering, centre):
		dom = method.param_info[name].within_radius_dom(selected_vals, instance, radius)
		selected_vals[name] = dom.random_value(selected_vals)
		logger.info("pick %s=%s", name, selected_vals[name].pretty)


	return [  selected_vals[name] for name in method.info.ordering ]


def is_in_inside(radius, centre, point):
	""" True if the ecuidean distance from the centre is less then the specified radius """
	summed =sum(  (c.distance(p)) for (p, c) in zip(point, centre) )
	# might be faster to do summed <= radius ** 2
	return math.sqrt(summed) <= radius
