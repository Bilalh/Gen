from lib import chain_lib

import itertools
from pprint import pprint


class SetPoint(object):
	"""Point which is a set param"""
	def __init__(self, num_elems):
		super(SetPoint, self).__init__()
		self.num_elems = num_elems
		self.size = 2 ** self.num_elems - 1

	def random_point(self):
		u = chain_lib.uniform_int(0, self.size)
		# print(bin(u))
		# print(u)
		# print(self.as_list(u))
		# return (self.as_list(u))
		return u

	def as_list(self, bits):
		return [ i for i in range(self.num_elems) if bits >> i & 1 == 1]


def test(size, times):
	s = SetPoint(size)
	arr = []
	for k, g in itertools.groupby(sorted([ s.as_list(s.random_point()) for i in range(times) ])):
		arr.append( (len(list(g)), k) )

	pprint(sorted(arr))
	print(len(arr))