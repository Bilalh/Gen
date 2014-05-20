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
		"""Should only be called once per race"""
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


class ExponentialTimeout(Timeout):

	def __init__(self, total, secs_per_race, num_models, method):
		super(ExponentialTimeout, self).__init__(secs_per_race, num_models, method)
		self.total=total

	def time_per_model(self):
		finished = self.method._current_iteration_no_fail + 1
		if finished <= self.total:
			time_per_model = self.max_time_per_model / ( 2 ** (self.total - finished) )
		else:
			time_per_model = self.max_time_per_model

		logger.info("ExponentialTimeout: time_per_model: %s finished_iters: %s  total_to_be_used:%s",
			time_per_model, finished, self.total)
		if time_per_model < 1:
			raise RuntimeError("""
				time_per model has to be greater then 1, is:{}.
				Using {} iterations, current:{}
				""".format(time_per_model, self.total, finished) )
		else:
			return time_per_model

class DynamicTimeout(Timeout):
	""" Starts off with a small timeout and increases from there """

	def __init__(self, secs_per_race, num_models, method):
		super(DynamicTimeout, self).__init__(secs_per_race, num_models, method)
		self.previous_times = []
		self.useless_count = 0
		self.first_time=True

	def time_per_model(self):
		"""
		Each time is used twice
		if the instance gives us no new infomation then
		"""
		if self.previous_times ==[]:
			self.previous_times.append(1)
			logger.info("First run using 1 second")
			return self.previous_times[-1]

		if self.first_time:
			new_timeout = self.previous_times[-1]
			self.previous_times.append(new_timeout)
			self.first_time=False
			return self.previous_times[-1]

		prev_quality = self.method.get_quailty(self.method.data_points[-1])
		if prev_quality ==1:
			self.useless_count+=1
			mult=min(1 + 0.3 * self.useless_count, 10)
			new_timeout = math.ceil(self.previous_times[-1] * mult)
		else:
			self.useless_count=0
			mult = (1.1 + 1 * prev_quality )
			new_timeout = math.ceil(self.previous_times[-1] * mult)

		if new_timeout > self.max_time_per_model:
			new_timeout = self.max_time_per_model
		self.previous_times.append(new_timeout)

		logger.info("Using %s as time per model (mult used %s) (Limit %s) (useless_count %s) (prev_quality %0.03f",
			self.previous_times[-1], mult, self.max_time_per_model, self.useless_count, prev_quality)

		self.first_time = True
		return self.previous_times[-1]





