from docopt import docopt
import re
import os

import limit
import pprint

import logging
logger = logging.getLogger(__name__)


def parse_arguments(doc, *, version):
	limiters = {
		"time": limit.TimeLimit,
		"iterations": limit.IterationsLimit
	}

	arguments = docopt(doc, version=version)

	for l in limiters:
		if arguments[l]:
			limiter_s = limiters[l]
		del arguments[l]

	logger.debug(arguments)
	logger.debug(limiter_s)

	# Convert ints to ints
	for key in re.findall(r"(--[_a-zA-Z]+)=<int>", doc):
		if arguments[key]:
			arguments[key] = int(arguments[key])

	# Convert dirs to abspath
	for (key, _) in re.findall(r"(--[_a-zA-Z]+)=<(dir|file)>", doc):
		if arguments[key]:
			arguments[key] = os.path.abspath(os.path.expanduser(arguments[key]))

	# remove -- from the start and <> around positional arguments
	options = { k.strip('<>').replace('--', ''): v for (k, v) in arguments.items()  }
	logger.info(pprint.pformat(options))

	options['limit'] = int(options['limit'])

	limiter = limiter_s(options['limit'])

	return (options, limiter)