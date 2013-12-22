from .chain_lib import copydoc

from abc import ABCMeta, abstractmethod
import time


class Limit(metaclass=ABCMeta):
	""" Limit for a algorithm based on some criteria """

	@abstractmethod
	def start(self, start):
		pass

	@abstractmethod
	def continue_running(self, method):
		""" Return true if the Algorithm should continue running"""


class TimeLimit(Limit):
	""" Limit total wall time """
	def __init__(self, seconds):
		super(TimeLimit, self).__init__()
		self.seconds = seconds

	def start(self):
		self.start_time = time.time()

	@copydoc(Limit.continue_running)
	def continue_running(self, method):
		return ( time.time() - self.start_time < self.seconds )


class IterationsLimit(Limit):
	""" Limit number of iterations """
	def __init__(self, iterations):
		super(IterationsLimit, self).__init__()
		self.iterations = iterations

	def start(self):
		self.current_iteration = 0

	@copydoc(Limit.continue_running)
	def continue_running(self, method):
		res = self.current_iteration < self.iterations
		self.current_iteration += 1
		return res


class CpuLimit(Limit):
	""" Limit total cpu time """
	def __init__(self, seconds):
		super(IterationsLimit, self).__init__()
		self.seconds = seconds

	def start(self):
		self.current = 0

	@copydoc(Limit.continue_running)
	def continue_running(self, method):
		if method.prev_timestamp:
			# FIXME Finish
			pass

		return self.current < self.seconds

