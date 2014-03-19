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
from pprint import pformat, pprint

import functools

logger = logging.getLogger(__name__)


@functools.total_ordering
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

    def __lt__(self, other):
        return self.point.__lt__(other.point)

    @abstractmethod
    def distance(self, other_dom):
        "  distance squared between the two domains, for use in euclidean "
        raise NotImplementedError("distance not done yet")

    @classmethod
    def from_json_dict(cls, d):
        """ toplevel should be  {tag: type } """
        raise NotImplementedError()


@functools.total_ordering
class Int(Instance):
    def __init__(self, point, pretty, safe):
        super(Int, self).__init__(point, pretty, safe)

    def distance(self, other_dom):
        "  distance squared between the two domains, for use in euclidean "
        if not isinstance(other_dom, self.__class__):
            raise ValueError("other dom must of %s" % self.__class__.__name__)

        return (other_dom.point - self.point) ** 2

    @classmethod
    def from_json_dict(cls, d):
        raise NotImplementedError()


@functools.total_ordering
class TypeInt(Instance):
    def __init__(self, point, pretty, safe):
        super(TypeInt, self).__init__(point, pretty, safe)

    def distance(self, other_dom):
        "  distance squared between the two domains, for use in euclidean "
        if not isinstance(other_dom, self.__class__):
            raise ValueError("other dom must of %s" % self.__class__.__name__)

        ## FIXME check if this calculation is want I want
        return sum(  ( t - o) ** 2 for (t, o) in zip( self.point, other_dom.point) )

    @classmethod
    def from_json_dict(cls, d):
        raise NotImplementedError()


@functools.total_ordering
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

    def __lt__(self, other):
        return self.point.items().__lt__(other.point.items())

    @classmethod
    def from_json_dict(cls, dom):
        mappings = jmatch(dom, "function", "values" )

        def f(mapping):

            [come, to] = [ process_literal(lit) for lit in jmatch(mapping, "mapping", "value") ]
            return (come.point, to)

        res = dict([ f(m) for m in mappings ])

        return cls.construct(res)

    @staticmethod
    def construct(func):
        kv = [ "{} --> {}".format(k, v.pretty) for (k, v) in sorted(func.items()) ]
        pretty = "function( {} )".format( ", ".join(kv) )

        kv_safe = [ "{}_{}".format(k, v.safe) for (k, v) in sorted(func.items()) ]
        safe = "F__{}__".format( ",".join(kv_safe) )
        return Func(point=func, pretty=pretty, safe=safe)



@functools.total_ordering
class Set(Instance):
    def __init__(self, point, pretty, safe):
        super(Set, self).__init__(point, pretty, safe)

    def distance(self, other_dom):
        "  distance between the two domains, for use in euclidean "
        #Like function ignore values in the larger one

        def f(x1, x2):
            if x1 is None or x2 is None:
                return 0
            else:
                return x1.distance(x2) ** 2


        parts = [ f(x1, x2) for (x1, x2) in it.zip_longest(self.point, other_dom.point) ]
        return sum(parts)

    @classmethod
    def from_json_dict(cls, d):

        values_dom = jmatch(d, 'set', 'values', 'value')

        values = [ json_instance_dispatcher(v_dom) for v_dom in values_dom ]

        resulting=[ (v.pretty, v.safe) for v in values ]
        if resulting:
            (pretty, safe) = zip(*resulting)
        else:
            (pretty, safe) = ("", "")

        pretty = "{%s}" % ", ".join(pretty)
        safe = "S__{}__".format(",".join(safe) )

        return Set(values, pretty, safe)


def create_param_essence(essence_file, output_dir):
    """ Create a essence spec of the params then refine it """

    out = Path(output_dir)

    sys.stdout.flush()
    sys.stderr.flush()

    subprocess.Popen([
        chain_lib.wrappers("create_param_essence.sh"), essence_file, str(out)
    ]).communicate()


def pre_create_all_param_solutions_from_essence(generated_dir, givens_names, param_info):

    base_path = Path(generated_dir)

    essence = base_path / 'essence_param_find.essence'
    eprime = base_path / 'essence_param_find.eprime'
    timeout = str(86400)  # FIXME choose better timeout

    data_path = base_path / "all_sols_data"
    solutions_path = base_path / "all_sols"
    params_path = base_path / "all_sols_params"

    for e in [data_path, solutions_path, params_path]:
        if not e.exists():
            e.mkdir()

    all_givens_vals = [  param_info[name].all_values_({}) for name in givens_names ]

    for givens_vals in it.product(*all_givens_vals):
        # Givens should be Independent from each other
        givens = list(zip(givens_names, givens_vals))
        (param_string, param_name) = chain_lib.create_param_file(givens)
        chain_lib.write_param(str(params_path), param_string, param_name)



    (param_string, param_name) = chain_lib.create_param_file(givens)

    current_env= os.environ.copy()
    current_env["GENERATED_OUTPUT_DIR"] = str(data_path)
    current_env["GENERATED_SOLUTIONS_DIR"] = str(solutions_path)

    current_env["PARAMS_DIR"] = str(params_path)

    use_previous = False
    if use_previous and (solutions_path / "solutions.count").exists():
        pass
    else:
        subprocess.Popen([
                chain_lib.wrappers("pre_create_all_params_from_essence_par_no_up.sh"),
                timeout, str(essence), str(eprime),
        ], env=current_env ).communicate()


    try:
        with ( data_path / "total.time" ).open() as f:
            time_taken=float(f.read().rstrip())

        with ( solutions_path / "solutions.counts" ).open() as f:
            parts = [ line.strip().split(' ') for line in f.readlines() ]
            solutions_count = 0
            for (i, (v, _)) in enumerate(parts):
                j = int(v)
                parts[i][0] = j + solutions_count
                solutions_count += j

            pprint(parts)

    except IOError:
        raise FailedToCreateAllSolutions()

    logger.info("Time taken to generate all solution %s, count %s", time_taken, solutions_count)
    return (time_taken, parts)


class FailedToCreateAllSolutions(Exception):
    pass


def create_param_from_essence(output_dir, givens):
    base_path = Path(output_dir) / 'param_gen'

    datee = calendar.datetime.datetime.now()
    logger.info("Make param %s", datee.isoformat())
    now = str(int(datee.timestamp()))

    (param_string, param_name) = chain_lib.create_param_file(givens)
    random_seed = chain_lib.uniform_int(1, (2 ** 32) - 1)

    essence = base_path / 'essence_param_find.essence'
    eprime = base_path / 'essence_param_find.eprime'
    timeout = str(300)  # FIXME choose better timeout


    # reuse previous data
    # glob_test = list(base_path.glob("**/" + param_name + ".param"))
    glob_test = []

    if glob_test:
        out = glob_test[0].parent
        logger.info("Using previous used param created in %s", out)
        solution = (out / (essence.stem + "-" + param_name) ).with_suffix('.solution')
        solution_json = solution.with_suffix('.json')
    else:
        out = base_path / now
        if not out.exists():
            out.mkdir()

        current_env= os.environ.copy()
        current_env["GENERATED_OUTPUT_DIR"] = str(out)
        current_env["TIMEOUT5_FILE"] = str(out / "timeout_file")


        param = (out / param_name).with_suffix('.param')

        chain_lib.write_param(str(out), param_string, param_name)
        solution = (out / (essence.stem + "-" + param.stem) ).with_suffix('.solution')
        solution_json = solution.with_suffix('.json')

        subprocess.Popen([
            chain_lib.wrappers("create_param_from_essence.sh"),
            str(essence), str(eprime), str(param), timeout, timeout, str(random_seed)
        ], env=current_env ).communicate()

    try:
        with ( out / "total.time" ).open() as f:
            time_taken=float(f.read().rstrip())

    except IOError:
        raise FailedToGenerateParamExeception()

    try:
        with solution_json.open() as f:
            raw_json = json.loads(f.read())

    except IOError:
        raise FailedToGenerateParamExeception()


    param_map = dict([ json_to_param_instance(letting) for letting in raw_json['lettings'] ])

    return (param_map, time_taken)


class FailedToGenerateParamExeception(Exception):
    pass


def json_to_param_instance(data):
    name = data['name']
    logger.info("name:%s", name)

    kind = jmatch(data['domain'][0], 'value')[0]
    instance = json_instance_dispatcher(kind)

    logger.info("name:%s, instance:%s", name, instance)

    return (name, instance)


def json_instance_dispatcher(kind):
    dispatch ={
    "function": Func.from_json_dict,
    "literal": process_literal,
    "set": Set.from_json_dict
    }

    instance = dispatch[kind['tag']](kind)
    return instance


def jmatch(d, *names):
    """ Similar to xmatch """
    lnames = len(names)
    for (i, tagname) in enumerate(names):
        if i == lnames - 1 and isinstance(d, list):
            return [ jmatch(v, tagname)[0] for v in d ]

        if tagname != '*' and d['tag'] != tagname:
            raise NotTagFoundExeception("{} not a tag of {}".format(tagname, pformat(d)))
        d = d['children']

        if i != lnames - 1 and len(d) == 1:
            d = d[0]

    return d


class NotTagFoundExeception(Exception):
    pass


def process_literal(lit):
    prim = jmatch(lit, 'literal')[0]['primitive']
    assert len(prim) == 1
    u = prim['int']
    pretty = "{}".format(u)
    return Int(point=u,  pretty=pretty, safe=pretty )

