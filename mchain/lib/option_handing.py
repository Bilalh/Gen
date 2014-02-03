from lib import limit

from collections import namedtuple
from docopt import docopt
import json
import logging
import os
import pprint
import re


logger = logging.getLogger(__name__)
Info=namedtuple("Info", ['ordering', 'givens'])


def parse_arguments(doc, *, version):
	""" Parse docopt help and do some type convertion  """

	arguments = docopt(doc, version=version)

	if arguments['json']:
		with open(arguments['<file>']) as fp:
			options = json.load(fp)
			limiter = getattr(limit, options['limiter'])(options['limit'])
			del options['limiter']
			info = Info(*options['info'])
			del options['info']

			return (options, limiter, info)

	limiters = {
		"time": limit.TimeLimit,
		"iterations": limit.IterationsLimit,
		"cpu": limit.CpuLimit
	}


	to_delete = ['json', '<file>']
	for l in to_delete:
		del arguments[l]

	for l in limiters:
		if arguments[l]:
			limiter_s = limiters[l]
		del arguments[l]

	logger.debug(arguments)
	logger.debug(limiter_s)

	# Convert ints to ints
	for key in re.findall(r"(--[_a-zA-Z]+)=<int>", doc):
		if arguments[key] is not None:
			arguments[key] = int(arguments[key])


	# Convert bools to bools
	for key in re.findall(r"  (--[_a-zA-Z]+)=<bool>", doc):
		if arguments[key] is not None:
			# print(arguments[key])
			if arguments[key].lower() == "true":
				arguments[key] = True
			elif arguments[key].lower() == "false":
				arguments[key] = False
			else:
				raise RuntimeError("{} has to be (true|false)".format(key))


	for (key, str_values) in re.findall(r"  (--[_a-zA-Z]+)=<(\w+\!.*?)>", doc):
		values = set(str_values.split("!"))
		if arguments[key] not in values:
			raise RuntimeError("{} has to be in {}".format(key, values))



	# Convert dirs to abspath
	for (key, kind) in re.findall(r"(--[_a-zA-Z]+)=<(dir|file)>", doc):
		if arguments[key] is not None:
			if arguments[key].strip() == "":
				raise RuntimeError("{} can not be empty if specifed".format(key))

			arguments[key] = os.path.abspath(os.path.expanduser(arguments[key]))
			if kind == "dir" and os.path.exists(arguments[key]) and not os.path.isdir(arguments[key]):
				raise NotADirectoryError("{}".format(arguments[key]))

			if kind == "file" and not os.path.isfile(arguments[key]):
				raise RuntimeError("{} is not a file".format(arguments[key]))



	# remove -- from the start and <> around positional arguments
	options = { k.strip('<>').replace('--', ''): v for (k, v) in arguments.items()  }
	logger.info(pprint.pformat(options))

	options['limit'] = int(options['limit'])

	limiter = limiter_s(options['limit'])

	with open(options['info']) as fp:
		info = Info(**json.load(fp))
		del options['info']


	return (options, limiter, info)

