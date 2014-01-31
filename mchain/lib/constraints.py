from lib import chain_lib
from lib import domains
from lib import instances

from abc import ABCMeta, abstractmethod
from pprint import pprint

import json
import logging
import itertools as it


logger = logging.getLogger(__name__)



class Constraint(metaclass=ABCMeta):
    def __init__(self):
        super(Constraint, self).__init__()

    @abstractmethod
    def process(self, selected_vals, domain, extra=None):
        pass


class FuncForallLessThenEq(Constraint):
    def __init__(self, target):
        super(FuncForallLessThenEq, self).__init__()
        self.target = target

    def process(self, selected_vals, domain, from_to):
        if not isinstance(domain, domains.Func):
            raise ValueError("not Function Domain %s" % domain)

        (from_vals, tos_doms) = from_to
        target_instance = selected_vals[self.target]

        if not isinstance(target_instance, instances.Func):
            raise ValueError("target %s not a function %s" % (self.target, target_instance) )

        if len(target_instance.point) != len(from_vals):
            raise ValueError('Functions not same length')

        # assumming int function
        def process(target_val, to_domain):
            if not isinstance(target_val, instances.Int):
                raise ValueError("target_val not a int instance %s" % (target_val) )

            [low, high] = [to_domain.resolve(d, selected_vals).point for d in to_domain.low_high]

            if target_val.point < high:
                high = target_val.point
            to_domain.low_high = (low, high)

            return to_domain

        return [ process(target_instance.point[f], t) for (f, t) in zip(from_vals, tos_doms)]
