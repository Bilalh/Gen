# -*- coding: utf-8 -*-

import logging
logger = logging.getLogger(__name__)

from enum import Enum, unique

import sys

@unique
class K(Enum):
    refineCompact = 0,
    refineAll     = 1,
    refineRandom  = 2,
    refineParam   = 3,
    savilerow     = 4,
    translateUp   = 5,
    validate      = 6,
    validateOld   = 7
    kindAny       = 8


class Commands(object):
    def __init__(self, *, refine_compact, refine_all, refine_random, refine_param,
                 savilerow, translate_up, validate):
        super(Commands, self).__init__()
        self.refine_compact = (K.refineCompact, refine_compact)
        self.refine_all=(K.refineAll, refine_all)
        self.refine_random = (K.refineRandom, refine_random)
        self.refine_param = (K.refineParam, refine_param)
        self.savilerow = (K.savilerow, savilerow)
        self.translate_up = (K.translateUp, translate_up)
        self.validate = (K.validate, validate)

    def kind_to_template(self, kind):
        d = {
        K.refineCompact: self.refine_compact,
        K.refineAll: self.refine_all,
        K.refineRandom: self.refine_random,
        K.refineParam: self.refine_param,
        K.savilerow: self.savilerow,
        K.translateUp: self.translate_up,
        K.validate: self.validate,
        }

        if kind in d:
            return d[kind]
        else:
            print("%s not a vaild kind" % d )
            sys.exit(6)

    def refine_log_follow(self, kind):
        raise NotImplementedError()



class ConjureOld(Commands):
    def __init__(self):
        super(ConjureOld, self).__init__(
                refine_compact="""
                conjureOld
                    --mode compact
                    --in-essence {essence}
                    --out-eprime {eprime}
                    --timelimit  {itimeout}
                """,

                refine_all="""
                conjureOld
                    --mode df
                    --in-essence       {essence}
                    --output-directory {outdir}
                    --timelimit        {itimeout}
                """,

                refine_random="""
                conjureOld
                    --mode random
                    --in-essence {essence}
                    --out-eprime {eprime}
                    --timelimit  {itimeout}
                """,

                refine_param="""
                conjureOld
                    --mode       refineParam
                    --in-essence       {essence}
                    --in-eprime        {eprime}
                    --in-essence-param {essence_param}
                    --out-eprime-param {eprime_param}
                    --timelimit        {itimeout}
                """,

                savilerow="""
                savilerow  -mode Normal
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
                conjureOld
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
                conjureOld --mode validateSolution
                             --in-essence       {essence}
                             --in-solution      {essence_solution}
                             --in-param         {essence_param}
                             --timelimit        {itimeout}
                """

                )
        self.sovlve_cmds=[self.refine_param, self.savilerow,
                            self.translate_up, self.validate]



class ConjureNew(Commands):
    def __init__(self):
        super(ConjureNew, self).__init__(
                refine_compact="""
                conjureNew {essence}
                    -q f -a c
                    --output-directory '{outdir}'
                    --numbering-start  {index}
                    --limit-time       {itimeout}
                    --seed             {seed}
                    --log-choices
                    --log-level=logfollow
                """,

                refine_all="""
                conjureNew             '{essence}'
                    -q f -a x
                    --output-directory '{outdir}'
                    --limit-time       {itimeout}
                    --seed             {seed}
                    --log-choices
                """,

                refine_random="""
                conjureNew {essence}
                    -q f -a r
                    --output-directory '{outdir}'
                    --numbering-start  {index}
                    --limit-time       {itimeout}
                    --seed             {seed}
                    --log-choices
                    --log-level=logfollow
                """,

                refine_param="""
                conjureNew refine-param
                    --eprime        '{eprime}'
                    --essence-param '{essence_param}'
                    --eprime-param  '{eprime_param}'
                """,

                savilerow="""
                savilerow  -mode Normal
                    -in-eprime                   '{eprime}'
                    -in-param                    '{eprime_param}'
                    -out-minion                  '{minion}'
                    -out-solution                '{eprime_solution}'
                    -out-info                    '{eprime_info}'
                    -run-solver
                    -timelimit                   {mstimeout}
                    -solver-options '-timelimit {itimeout}'
                """,

                translate_up="""
                conjureNew translate-solution
                    --eprime           '{eprime}'
                    --essence-param    '{essence_param}'
                    --eprime-solution  '{eprime_solution}'
                    --essence-solution '{essence_solution}'
                """,

                validate="""
                conjureNew validate-solution
                             --essence      '{essence}'
                             --param        '{essence_param}'
                             --solution     '{essence_solution}'
                """
                )


        self.sovlve_cmds=[self.refine_param, self.savilerow, self.translate_up,
                            self.validate]

    def refine_log_follow(self, kind):
        if kind not in [K.refineCompact, K.refineRandom]:
            print("Not a vaild kind for log-following {}".format(kind), file=sys.stderr)
            sys.exit(1)
        (_, template) = self.kind_to_template(kind)
        template += "\n--choices {saved_choices}"
        return template



