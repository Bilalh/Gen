from .chain_lib import copydoc

from abc import ABCMeta, abstractmethod
import time


class Limit(metaclass=ABCMeta):
	""" Limit for a algorithm based on some criteria """

	@abstractmethod
	def start(self, start):
		pass

	@abstractmethod
	def continue_running(self):
		""" Return true if the Algorithm should continue running"""


class TimeLimit(Limit):

	def __init__(self, seconds):
		super(TimeLimit, self).__init__()
		self.seconds = seconds

	def start(self):
		self.start_time = time.time()

	@copydoc(Limit.continue_running)
	def continue_running(self):
		return ( time.time() - self.start_time < self.seconds )


class IterationsLimit(Limit):

	def __init__(self, iterations):
		super(IterationsLimit, self).__init__()
		self.iterations = iterations

	def start(self):
		self.current_iteration = 0

	@copydoc(Limit.continue_running)
	def continue_running(self):
		res = self.current_iteration < self.iterations
		self.current_iteration += 1
		return res

