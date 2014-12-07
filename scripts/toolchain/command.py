# -*- coding: utf-8 -*-

import logging
logger = logging.getLogger(__name__)

from enum import Enum, unique

@unique
class K(Enum):
    refineCompact = 0,
    refineRandom  = 2,
    refineParam   = 3,
    savilerow     = 4,
    translateUp   = 5,
    validate      = 6


class Commands(object):
	"""Functions for Old Conjure"""
	def __init__(self, *, refine_compact, refine_random, refine_param, savilerow, translate_up, validate):
		super(Commands, self).__init__()
		self.refine_compact = (K.refineCompact, refine_compact)
		self.refine_random = (K.refineRandom, refine_random)
		self.refine_param = (K.refineParam, refine_param)
		self.savilerow = (K.savilerow, savilerow)
		self.translate_up = (K.translateUp, translate_up)
		self.validate = (K.validate, validate)


conjure_old = Commands(
refine_compact="""
time conjure
    --mode compact
    --in-essence {essence}
    --out-eprime {eprime}
    --timelimit  {itimeout}
""",

refine_random="""
time conjure
    --mode random
    --in-essence {essence}
    --out-eprime {eprime}
    --timelimit  {itimeout}
""",

refine_param="""
time conjure
    --mode       refineParam
    --in-essence       {essence}
    --in-eprime        {eprime}
    --in-essence-param {essence_param}
    --out-eprime-param {eprime_param}
    --timelimit        {itimeout}
""",

savilerow="""
time savilerow  -mode Normal
    -in-eprime                   {eprime}
    -in-param                    {eprime_param}
    -out-minion                  {minion}
    -out-solution                {eprime_solution}
    -out-info                    {eprime_info}
    -run-solver
    -timelimit                   {mstimeout}
    -solver-options '-timelimit {itimeout}'
""",

translate_up="""
time conjure
    --mode translateSolution
    --in-essence            {essence}
    --out-solution          {essence_solution}
    --in-eprime             {eprime}
    --in-eprime-solution    {eprime_solution}
    --in-essence-param      {essence_param}
    --in-eprime-param       {eprime_param}
    --timelimit             {itimeout}
""",

validate="""
time conjure --mode validateSolution
             --in-essence       {essence}
             --in-solution      {essence_solution}
             --in-param         {essence_param}
             --timelimit        {itimeout}
"""
)
