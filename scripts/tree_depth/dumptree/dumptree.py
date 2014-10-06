#!/usr/bin/env python3
# -*- coding: utf-8 -*-
from __future__ import print_function, division, absolute_import


from collections import deque
from pprint import pprint, pformat
import util

import logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.ERROR)


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

		self.left, self.right, self.parent = None, None, None
		self.depth = None

	def set_parent(self, parent):
		self.parent = parent
		if self.parent:
			self.depth = self.parent.depth + 1
		else:
			self.depth=0

	def process(self, tree):
		if not self.parent:
			assert self.number == 0
			return


		if not self.actions:
			return

		self.left = NullNode(self)

		prev = self
		for bt, tu in iter_many(self.actions, len(self.actions), 2):
			while tu[0] != prev.assigned[0]:
				prev = prev.parent
			prev.right = NullNode(prev)


		tree.current = prev.right

	def to_dot(self, fh):
		self._dot_node(fh)
		if self.left:
			fh.write('\t{} -> {} [label="{}"];\n'.format(
				self.number, self.left.number, self._str_assgined() ))

		if self.right:
			fh.write('\t{} -> {} [label="{}"];\n'.format(
				self.number, self.right.number, self._str_assgined(sym="≠") ))

	def _dot_node(self, fh):
		fh.write('\t{0} [label="{0}" ]\n'.format(self.number) )

	def _str_assgined(self, sym="="):
			if self.number == 0:
				return ""
			name = self.assigned[0]
			name = name.replace("AnyVarRef:Bool:Bool:", "b_")
			name = name.replace("AnyVarRef:LongRange", "l_")
			return "{} {} {}".format(name, sym, self.assigned[1])

	def __repr__(self):
		def f(v):
			if v: return v.number
			else: return "ø"

		if self.assigned:
			a=self._str_assgined()
		else:
			a=""

		return "{}({:<3}, depth {:<3}  L {:<3}  R {:<3}  P {:<3}) {} A {}".format(
			self.__class__.__name__, self.number, self.depth,
			f(self.left), f(self.right), f(self.parent), a, not not self.actions )


class NullNode(Node):
	"""
	Node with no children
	"""
	__counter = -1

	def __init__(self, parent):
		super(NullNode, self).__init__(NullNode.__counter)
		NullNode.__counter = NullNode.__counter - 1
		self.set_parent(parent)

	def _dot_node(self, fh):
		fh.write('\t{0} [shape=point] \n'.format(self.number, self.depth) )


class Tree():
	def __init__(self):
		super(Tree, self).__init__()

	def parse(self, fp):
		""" returns a tree of Node (s)"""

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

		# for last node
		node.left = NullNode(node)


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


		if isinstance(self.current, NullNode):
			n.set_parent(self.current.parent)
			n.parent.right = n
			# self.current.parent = None # might help gc
		else:
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
			node.assigned = (name, int(snum))
			if node.parent.right != node:
				node.parent.left = node
		else:
			raise ValueError("Not handled, value: " + value)

	def parse_SearchAction(self, value, node):
		if value == 'bt':
			if node:
				node.actions.append("bt")
		else:
			raise ValueError("Not handled, value: " + value)

	def debug_print(self):
		# for ix in self.index:
		# 	pprint(ix.__dict__)
		pprint(self.index)

	def to_dot(self, fp):

		with open(fp, "w") as fh:
			fh.write('digraph G{\n\tgraph [ordering="out"];\n')

			d = deque()
			d.append(self.root)
			while d:
				cur = d.pop()
				cur.to_dot(fh)
				if cur.left:
					d.append(cur.left)
				if cur.right:
					d.append(cur.right)

			fh.write('}')


# Tree().parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/simple/simple.eprime.minion-tree")
t= Tree()
# t.parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/bibd/puget11.param.minion-tree")
t.parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/aa.param.minion-tree")
t.debug_print()
t.to_dot("a.dot")

