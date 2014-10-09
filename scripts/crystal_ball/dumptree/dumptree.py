#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Parses a dumptree from  minion -dumptree

from __future__ import print_function, division, absolute_import

from collections import deque
from pprint import pprint, pformat

import logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(name)s:%(lineno)d:%(funcName)s: %(message)s', level=logging.ERROR)

import csv
import json

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("dumptree", help='dumptree from minion -dumptree')
parser.add_argument("csv",      help='csv output location')
parser.add_argument('-p', action='store_true', dest='print_nodes', help='print nodes')
parser.add_argument('--dot',  help='output the tree as a .dot file, for visualisation')
parser.add_argument('--meta-json',  help='output metadata as json')
parser.add_argument('--meta-csv',   help='output metadata as csv')



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
		self.backtracked = None

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
			while isinstance(prev, SolutionNode) or tu[0] != prev.assigned[0]:
				prev = prev.parent
			prev.right = NullNode(prev)

		self.backtracked = prev
		tree.current = prev.right


	def to_dot(self, fh):
		self._dot_node(fh)
		if self.left:
			fh.write('\t{} -> {} [label=" {}"];\n'.format(
				self.number, self.left.number, self._str_assgined() ))

		if self.right:
			fh.write('\t{} -> {} [label=" {}"];\n'.format(
				self.number, self.right.number, self._str_assgined(sym="≠") ))

	def _dot_node(self, fh):
		if self.backtracked:
			extra = ",color=blue"
		else:
			extra = ""
		fh.write('\t{0} [label="{0}" {1}]\n'.format(self.number, extra) )

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

		return "{}({:<3}, depth {:<3}  L {:<3}  R {:<3}  P {:<3}) {} BT {}".format(
			self.__class__.__name__, self.number, self.depth,
			f(self.left), f(self.right), f(self.parent), a,  f(self.backtracked))


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


class SolutionNode(Node):
	"""
	When a solution is found in opt problems, no children
	"""

	def __init__(self, parent, num):
		super(SolutionNode, self).__init__(num)
		self.set_parent(parent)
		self.parent.left = self

	def to_dot(self, fh):
		self._dot_node(fh)

	def _dot_node(self, fh):
		fh.write('\t{0} [shape=point,color=red] \n'.format(self.number, self.depth) )

	def _str_assgined(self, sym='='):
		return "Sol"


class Tree(object):
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

		def process_last_node():
			old=self.current
			if self.current:
				logger.info("current %s, assigned %s", self.current, self.current.assigned)
				self.current.process(self)
				logger.info("Processed  %s", pformat(old.__dict__))
				logger.info("current %s, assigned %s", self.current, self.current.assigned)


		max_depth = -1
		node = None
		for (tag, value) in lines:

			if tag == 'Node':
				process_last_node()
				node = self.parse_Node(value)
				if node.depth > max_depth:
					max_depth = node.depth

			elif tag == "Solution found with Value":
				process_last_node()
				num = len(self.index)
				node = SolutionNode(self.current, num)
				self.parse_node_after(node, num)

				if node.depth > max_depth:
					max_depth = node.depth

			elif tag == "SearchAssign":
				self.parse_SearchAssign(value, node)
			elif tag == "SearchAction":
				self.parse_SearchAction(value, node)
			else:
				self.meta[tag] = value.strip()

		# for last node
		self.current.left = NullNode(node)

		self.meta["Max Depth"] = max_depth


	def parse_Node(self, value):
		end_of_num = value.find(',')
		num = int(value[0:end_of_num])

		if self.current:
			logger.info("current %s, assigned %s", self.current, self.current.assigned)
		else:
			logger.info("current %s", self.current)

		node = Node(num)
		return self.parse_node_after(node, num)

	def parse_node_after(self, node, num):
		if self.current:
			logger.info("current %s, assigned %s", self.current, self.current.assigned)
		else:
			logger.info("current %s", self.current)

		self.index.append(node)
		assert self.index[num] == node

		logger.info("Parsed     %s", pformat(node.__dict__))

		if isinstance(self.current, NullNode):
			node.set_parent(self.current.parent)
			node.parent.right = node
			# self.current.parent = None # might help gc
		else:
			node.set_parent(self.current)


		if not self.root:
			self.root = node
		self.current = node

		return node


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

	def print_nodes(self):
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
				if isinstance(cur, SolutionNode):
					assert isinstance(cur.left, NullNode) or cur.left is None
					assert cur.right is None
					continue

				if cur.left:
					d.append(cur.left)
				if cur.right:
					d.append(cur.right)

			fh.write('}')

	def to_csv(self, fp):
		assert self.index
		data = []
		for n in self.index:
			depth_from, depth_to = "NA", "NA"
			if n.backtracked:
				depth_from = n.depth + 1
				depth_to = n.backtracked.depth
			data.append(dict(index=n.number, bt=not not n.backtracked,
						depth_from=depth_from, depth_to=depth_to, depth=n.depth))

		with open(fp, "w") as fh:
			temp=data[0].keys()
			fieldnames = ["index", "depth", "bt", "depth_from", "depth_to"]
			assert sorted(temp) == sorted(fieldnames)

			writer = csv.DictWriter(fh, delimiter=',', fieldnames=fieldnames )
			writer.writerow(  { fn: fn for fn in fieldnames} )
			for row in data:
				writer.writerow(row)

	def fix_meta(self):
		for ikey in ["Maximum RSS (kB)", "Solutions Found", "Total Nodes"]:
			self.meta[ikey] = int(self.meta[ikey])

		for rkey in ["First Node Time", "First node time", "Initial Propagate",
					 "Parsing Time", "Setup Time", "Solve Time", "Total System Time",
					 "Total Time", "Total Wall Time"]:
			self.meta[rkey] = float(self.meta[rkey])

		for bkey in ["Problem solvable?"]:
			self.meta[bkey] = self.meta[bkey].lower() == "yes"

	def save_meta_json(self, fp):
		assert self.meta
		self.fix_meta()
		with open(fp, "w") as fh:
			json.dump(self.meta, fh)

	def save_meta_csv(self,fp):
		assert self.meta
		self.fix_meta()
		with open(fp, "w") as fh:
			fieldnames = sorted(self.meta.keys())
			writer = csv.DictWriter(fh, delimiter=',', fieldnames=fieldnames )
			writer.writerow(  { fn: fn for fn in fieldnames} )
			writer.writerow(self.meta)

if __name__ == "__main__":
	args = parser.parse_args()
	t= Tree()
	# t.parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/bibd/puget11.param.minion-tree")
	# t.parse("/Users/bilalh/CS/instancegen/scripts/tree_depth/aa.param.minion-tree")
	t.parse(args.dumptree)

	if args.print_nodes:
		t.print_nodes()

	t.to_csv(args.csv)

	if args.dot:
		t.to_dot(args.dot)

	if args.meta_json:
		t.save_meta_json(args.meta_json)

	if args.meta_csv:
		t.save_meta_csv(args.meta_csv)
