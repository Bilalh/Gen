#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import


class Printable(object):
	def __init__(self):
		super(Printable, self).__init__()

	def __repr__(self):
		return "{}({})".format(self.__class__.__name__,
			', '.join( key + "=" + str(getattr(self, key))
				for key in self.__dict__ if not key.startswith('_'))
				)