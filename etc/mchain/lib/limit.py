from .chain_lib import copydoc

from abc import ABCMeta, abstractmethod
import os
import time
import logging

logger = logging.getLogger(__name__)


class Limit(metaclass=ABCMeta):
	""" Limit for a algorithm based on some criteria """

	@abstractmethod
	def start(self, start):
		pass

	@abstractmethod
	def continue_running(self, method):
		""" Return true if the Algorithm should continue running"""

	def __repr__(self):
		return "{}({})".format(self.__class__.__name__,
			', '.join( key + "=" + str(getattr(self, key))
				for key in self.__dict__ if not key.startswith('_'))
				)


# cpu times are still reported in the stats
class TimeLimit(Limit):
	""" Limit total wall time """
	def __init__(self, seconds):
		super(TimeLimit, self).__init__()
		self.seconds = seconds

	def start(self):
		self.start_time = time.time()
		self.cputime_start = time.process_time()
		self.taken = 0

	@copydoc(Limit.continue_running)
	def continue_running(self, method, count_iter):

		self.taken = (time.time() - self.start_time
				+ time.process_time() - self.cputime_start
				+ method.extra_time )
		logger.info("%f second left, total:%f",  self.seconds - self.taken, self.seconds)
		return self.taken < self.seconds


class IterationsLimit(Limit):
	""" Limit number of iterations """
	def __init__(self, iterations):
		super(IterationsLimit, self).__init__()
		self.iterations = iterations

	def start(self):
		self.current_iteration = 0

	@copydoc(Limit.continue_running)
	def continue_running(self, method, count_iter):
		logger.info("Limit current iteration finished %s", self.current_iteration)
		if count_iter:
			self.current_iteration += 1
			logger.info("Limit iteration ++ %s", self.current_iteration)
		res = self.current_iteration <= self.iterations
		return res


class CpuLimit(Limit):
	""" Limit total cpu time """
	def __init__(self, seconds):
		super(CpuLimit, self).__init__()
		self.seconds = seconds
		self.cputime_start = time.process_time()

	def start(self):
		self.current = 0
		self.taken = 0

	@copydoc(Limit.continue_running)
	def continue_running(self, method, count_iter):
		if method.prev_timestamp:
			# TODO Assumes only param is run on each iteration
			timefile = os.path.join(method.settings.output_dir, "stats-" + method.settings.mode,
				method.prev_timestamp + ".total_solving_time")
			with open(timefile) as f:
				self.current += float(f.readline())

		self.taken = (self.current
				+ time.process_time() - self.cputime_start
				+ method.extra_time)
		logger.info("%f second left, total:%f",  self.seconds - self.taken, self.seconds)
		return self.taken < self.seconds

