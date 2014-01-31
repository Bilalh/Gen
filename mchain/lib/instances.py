from lib import chain_lib

from abc import ABCMeta, abstractmethod
from pprint import pprint

import json
import logging
import itertools as it


logger = logging.getLogger(__name__)


class Instance(metaclass=ABCMeta):

    """Instantiation of a domain"""
    def __init__(self, point, pretty, safe):
        super(Instance, self).__init__()
        self.point = point
        self.pretty = pretty
        self.safe = safe

    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
            ', '.join( key + "=" + str(getattr(self, key))
                for key in self.__dict__ if not key.startswith('_'))
                )

    def __str__(self):
        return "{}~{}".format(self.__class__.__name__, self.pretty)

    def to_json(self):
        return json.dumps(self, default=lambda o: o.__dict__, sort_keys=True, indent=4)

    def __eq__(self, other):
        self.pretty == other.pretty

    def __ne__(self, other):
        return not self.__eq__(other)

    @abstractmethod
    def distance(self, other_dom):
        raise NotImplemented("distance not done yet")


class Int(Instance):
    def __init__(self, point, pretty, safe):
        super(Int, self).__init__(point, pretty, safe)

    def distance(self, other_dom):
        "  distance squared between the two domains, for use in euclidean "
        if not isinstance(other_dom, self.__class__):
            raise ValueError("other dom must of %s" % self.__class__.__name__)

        return (other_dom.point - self.point) ** 2


class Func(Instance):
    def __init__(self, point, pretty, safe):
        super(Func, self).__init__(point, pretty, safe)

    def distance(self, other_dom):
        "  distance between the two domains, for use in euclidean "
        if not isinstance(other_dom, self.__class__):
            raise ValueError("other dom must of %s" % self.__class__.__name__)

        # logger.info("self %s other %s", self.pretty, other_dom.pretty)
        # logger.info("self %s other %s", self.point,  other_dom.point)

        def f(x1, x2):
            if x1 is None or x2 is None:
                return 0
            else:
                return (x1 - x2) ** 2


        parts = [ f(x1, x2) for (x1, x2) in it.zip_longest(self.point, other_dom.point) ]
        logger.debug("functin dist parts %s", parts)

        return sum(parts)


