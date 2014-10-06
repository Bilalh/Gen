#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import

from pprint import pprint, pformat
import util

import logging
logger = logging.getLogger(__name__)
# logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.WARN)
logging.basicConfig(format='%(lineno)-3d: %(message)s', level=logging.ERROR)


# Node#    BT    depth_from    depth_to
#  2       1     4             3
#  32      0     NA            Na


def iter_many(it, length, num):
	for i in range(0, length, num):
		yield (it[i:i + num])


class Node(object):
	def __init__(self, number):
		super(Node, self).__init__()
		self.number = number

		self.actions = []
		self.assigned = None

	def set_parent(self, parent):
		self.parent = parent
		if self.parent:
			self.depth = self.parent.depth + 1
		else:
			self.depth=0

	def process(self, tree):
		""" Deals with BT, current node"""
		if not self.parent:
			return


		prev = self
		for bt, tu in iter_many(self.actions, len(self.actions), 2):
			if tu[0] == self.assigned[0]:
				prev=prev.parent
				continue
			while tu[0] != prev.assigned[0]:
				prev = prev.parent
			logger.info("prev %s, assigned %s", prev,  prev.assigned)

		if prev != self:
			tree.current = prev

		logger.info("current %s, assigned %s", tree.current, tree.current.assigned)


	def __repr__(self):
		return "{}({})".format(self.__class__.__name__, self.number)


class Tree():
	def __init__(self):
		super(Tree, self).__init__()

	def parse(self, fp):
		""" returns a tree of Node (s)"""

		# fp="/Users/bilalh/CS/instancegen/scripts/tree_depth/simple/simple.eprime.minion-tree"
		self.current, self.root = None, None
		self.meta = {}
		self.index = []  # indexed by node number

		with open(fp) as f:
			raw_lines = [line.strip() for line in f.readlines()]

		# Get the `tag` the value for each line
		# skipping comments
		lines = [ line.partition(":")[0::2] for line in raw_lines
					if line and not line.startswith("#") ]

		node = None
		for (tag, value) in lines:
			if tag == 'Node':
				if node:
					logger.info("ParsedAtrr %s", pformat(node.__dict__))


				old=self.current
				if self.current:
					logger.info("current %s, assigned %s", self.current, self.current.assigned)
					self.current.process(self)
					logger.info("Processed  %s", pformat(old.__dict__))
					logger.info("current %s, assigned %s", self.current, self.current.assigned)

				node = self.parse_Node(value)

			elif tag == "SearchAssign":
				self.parse_SearchAssign(value, node)
			elif tag == "SearchAction":
				self.parse_SearchAction(value, node)
			else:
				self.meta[tag] = value.strip()

		# pprint(self.__dict__)


	def parse_Node(self, value):
		end_of_num = value.find(',')
		num = int(value[0:end_of_num])

		if self.current:
			logger.info("current %s, assigned %s", self.current, self.current.assigned)
		else:
			logger.info("current %s", self.current)

		n = Node(num)
		self.index.append(n)
		assert self.index[num] == n


		logger.info("Parsed     %s", pformat(n.__dict__))
		n.set_parent(self.current)

		if not self.root:
			self.root = n
		self.current = n

		return n

	def parse_SearchAssign(self, value, node):
		if "!=" in value:
			(name, snum) = value.split(" != ")
			node.actions.append( (name, "!=", int(snum))  )
		elif "=" in value:
			(name, snum) = value.split(" = ")
			# self.current.actions.append( (name, "=", int(snum))  )
			node.assigned = (name, int(snum) )
		else:
			raise ValueError("Not handled, value: " + value)

	def parse_SearchAction(self, value, node):
		if value == 'bt':
			if node:
				node.actions.append("bt")
		else:
			raise ValueError("Not handled, value: " + value)

	def debug_print(self):
		for ix in self.index:
			pprint(ix.__dict__)

	def to_dot(self, fp):
		with open(fp, "w") as f:
			f.write('digraph G{\n\tgraph [ordering="out"];')
			for ix in self.index:
				if ix.parent:
					f.write("\t{} -> {};\n".format(ix.number, ix.parent.number))

			f.write('}')


# Tree().parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/simple/simple.eprime.minion-tree")
t= Tree()
t.parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/bibd/puget11.param.minion-tree")
t.debug_print()
t.to_dot("a.dot")

