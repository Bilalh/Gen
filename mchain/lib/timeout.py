from abc import ABCMeta, abstractmethod
import logging
import math

logger = logging.getLogger(__name__)


class Timeout(metaclass=ABCMeta):

	def __init__(self, secs_per_race, num_models, method):
		super(Timeout, self).__init__()
		self.secs_per_race = secs_per_race
		self.num_models = num_models
		self.method = method
		self.max_time_per_model = int(math.ceil(self.secs_per_race / self.num_models))

	@abstractmethod
	def time_per_model(self):
		pass

	def __repr__(self):
		return "{}({})".format(self.__class__.__name__,
			', '.join( key + "=" + str(getattr(self, key))
				for key in self.__dict__ if not key.startswith('_'))
				)


class SimpleTimeout(Timeout):
	"""Always uses the same timeout"""
	def time_per_model(self):
		return self.max_time_per_model
