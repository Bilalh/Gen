# -*- coding: utf-8 -*-

import logging
logger = logging.getLogger(__name__)
logging.basicConfig(format='%(lineno)d:%(funcName)s: %(message)s',
        level=logging.INFO)

Conjure = """
time conjure --mode compact --in-essence {essence} --out-eprime {eprime} --timelimit {itimeout}
"""
ParamRefine="""
time conjure
    --mode       refineParam
    --in-essence       {essence}
    --in-eprime        {eprime}
    --in-essence-param {essence_param}
    --out-eprime-param {eprime_param}
    --timelimit        {itimeout}
"""

SR ="""
time savilerow  -mode Normal
    -in-eprime                   {eprime}
    -in-param                    {eprime_param}
    -out-minion                  {minion}
    -out-solution                {eprime_solution}
    -out-info                    {eprime_info}
    -run-solver
    -timelimit                   {mstimeout}
    -solver-options '-cpulimit {itimeout}'
"""

UP = """
time conjure
    --mode translateSolution
    --in-essence            {essence}
    --out-solution          {essence_solution}
    --in-eprime             {eprime}
    --in-eprime-solution    {eprime_solution}
    --in-essence-param      {essence_param}
    --in-eprime-param       {eprime_param}
    --timelimit             {itimeout}
"""

Vaildate= """
time conjure --mode validateSolution
             --in-essence       {essence}
             --in-solution      {essence_solution}
             --in-param         {essence_param}
             --timelimit        {itimeout}
"""
