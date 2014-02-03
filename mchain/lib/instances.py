from lib import chain_lib

from abc import ABCMeta, abstractmethod
from pprint import pprint
from pathlib import Path

import json
import logging
import itertools as it
import subprocess
import sys
import os
import calendar

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



def create_param_essence(essence_file, output_dir):
    """ Create a essence spec of the params then refine it """

    out = Path(output_dir) / "param_gen"

    sys.stdout.flush()
    sys.stderr.flush()

    subprocess.Popen([
        chain_lib.wrappers("create_param_essence.sh"), essence_file, str(out)
    ]).communicate()


def create_param_from_essence(output_dir):
    base_path = Path(output_dir) / 'param_gen'

    datee = calendar.datetime.datetime.now()
    logger.info("Make param %s", datee.isoformat())
    now = str(int(datee.timestamp()))

    out = base_path / now
    if not out.exists():
        out.mkdir()

    current_env= os.environ.copy()
    current_env["GENERATED_OUTPUT_DIR"] = str(out)
    current_env["TIMEOUT5_FILE"] = str(out / "timeout_file")

    random_seed = chain_lib.uniform_int(1, (2 ** 32) - 1)

    essence = str(base_path / 'essence_param_find.essence')
    eprime = str(base_path / 'essence_param_find.eprime')
    param = str(base_path / 'first.param')
    timeout = str(60)

    subprocess.Popen([
        chain_lib.wrappers("create_param_from_essence.sh"),
        essence, eprime, param, timeout, timeout, str(random_seed)
    ], env=current_env ).communicate()


