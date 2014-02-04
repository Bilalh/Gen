from lib import chain_lib
from lib import instances

from abc import ABCMeta, abstractmethod
from pprint import pprint

from collections import namedtuple

import itertools as it
import json
import subprocess
import sys
import os
import random
import logging

import itertools as it
import copy

logger = logging.getLogger(__name__)



class Domain(metaclass=ABCMeta):
    """Domain e.g int or function"""
    def __init__(self):
        super(Domain, self).__init__()
        self.constraints = []

    def __repr__(self):
        return "{}({})".format(self.__class__.__name__,
            ', '.join( key + "=" + str(getattr(self, key))
                for key in self.__dict__ if not key.startswith('_'))
                )

    def resolve(self, v, selected_vals):
        if isinstance(v, Ref):
            return v.resolve(selected_vals)
        return v

    @abstractmethod
    def random_value(self, selected_vals):
        """selected_vals is previous selected points"""
        pass

    @abstractmethod
    def all_values(self, selected_vals):
        raise NotImplemented("All values not done yet")

    @abstractmethod
    def within_radius_dom(self, selected_vals, instance, radius):
        pass


class Int(Domain):
    """Int domain in (low,high) inclusive """
    def __init__(self, low_high):
        super(Int, self).__init__()
        self.low_high = low_high

    def resolve(self, v, selected_vals):
        if isinstance(v, Ref):
            return v.resolve(selected_vals)
        else:
            pretty = "{}".format(v)
            return instances.Int(point=v, pretty=pretty, safe=pretty )


    def random_value(self, selected_vals):
        vs = [ self.resolve(v, selected_vals).point for v in self.low_high ]

        if vs[0] > vs[1] or vs[1] < vs[0]:
            raise NoValuesInDomainException("low_high: ", vs)

        u = chain_lib.uniform_int(*vs)
        pretty = "{}".format(u)
        return instances.Int(point=u,  pretty=pretty, safe=pretty )

    def within_radius_dom(self, selected_vals, instance, radius):
        (dlow, dhigh) = [ self.resolve(v, selected_vals).point for v in self.low_high ]

        (ilow, ihigh) = (instance.point - radius, instance.point + radius)

        low = max([ilow, dlow])
        high = min([ihigh, dhigh])

        return Int((low, high))

    def within_radius(self, selected_vals, instance, radius):
        (dlow, dhigh) = [ self.resolve(v, selected_vals).point for v in self.low_high ]

        (ilow, ihigh) = (instance.point - radius, instance.point + radius)

        low = max([ilow, dlow])
        high = min([ihigh, dhigh])

        u = chain_lib.uniform_int(low, high)
        pretty = "{}".format(u)
        return instance.Int(point=u,  pretty=pretty, safe=pretty )


    def all_values(self, selected_vals):
        low_high = [ self.resolve(v, selected_vals).point for v in self.low_high ]

        vs = range(low_high[0], low_high[1] + 1)
        return vs


# FIXME  use the form  1:instance at the moment
class Func(Domain):
    """Function Domain"""
    def __init__(self, fromm, tos, total=False):
        super(Func, self).__init__()
        self.total = total
        self.fromm = fromm
        self.tos = tos

    def random_value(self, selected_vals):
        # TODO  assumes total from ints at the momment
        assert len(self.fromm) == 1
        all_from = self.fromm[0].all_values(selected_vals)

        if len(self.tos) == 1:
            tos_domains = [ copy.deepcopy(self.tos[0]) for i in range(len(all_from)) ]
        else:
            assert len(all_from) == len(self.tos)
            tos_domains = [ copy.deepcopy(to) for to in self.tos ]

        return self.random_values_with_doms(selected_vals, all_from, tos_domains)

    def random_values_with_doms(self, selected_vals, froms, tos):
        for c in self.constraints:
            tos = c.process(selected_vals, self, (froms, tos) )

        logger.info("func(total) doms %s", list(zip(froms, [t.low_high for t in tos ] )) )

        tos = [ to_dom.random_value(selected_vals) for to_dom in tos ]
        res = dict(zip(froms, tos))

        logger.debug("rv %s", (froms, tos, res))

        kv = [ "{} --> {}".format(k, v.pretty) for (k, v) in sorted(res.items()) ]
        pretty = "function( {} )".format( ", ".join(kv) )

        kv_safe = [ "{}_{}".format(k, v.safe) for (k, v) in sorted(res.items()) ]
        safe = "F__{}__".format( ",".join(kv_safe) )

        di = instances.Func(point=res, pretty=pretty, safe=safe)
        # logger.info("point Part %s", pretty)
        return di


    def all_values(self, selected_vals):
        raise NotImplementedError("All values not done yet")

    def within_radius_dom(self, selected_vals, instance, radius):
        # TODO  assumes total from ints at the momment

        assert len(self.fromm) == 1
        assert len(self.tos) == 1
        dall_from = self.fromm[0].all_values(selected_vals)

        dkeys = set(dall_from)
        ikeys = set(instance.point.keys())

        common_from = dkeys & ikeys
        missing = dkeys - ikeys

        tos_mapping = {}
        for k in common_from:
            ki = instance.point[k]
            tos_mapping[k] = self.tos[0].within_radius_dom(selected_vals, ki, radius)


        for k in missing:
            tos_mapping[k] = self.tos[0]

        tos = [ tos_mapping[k] for k in sorted(tos_mapping) ]

        return Func(self.fromm, tos)


class Ref():
    """Refences to another var"""
    def __init__(self, name):
        super(Ref, self).__init__()
        self.name = name

    def resolve(self, selected_vals):
        if self.name not in selected_vals:
            raise UninitialisedVal("{} not initialised (ordering incorrect)".format(self.name))

        return selected_vals[self.name]

    def __repr__(self):
        return "Ref(name={})".format(self.name)


class NoValuesInDomainException(Exception):
    pass


class UninitialisedVal(Exception):
    """Thrown when a reference is unresolved"""
    pass


def next_ele(ele):
    next_ele = ele['children'][0]
    return (next_ele['tag'], next_ele)


def transform_json_domain_to_domain(data):
    dom = data[0]
    assert dom['tag'] == 'domain'

    (kind, kind_ele) = next_ele(dom)

    dispach = {
        "int": get_int_domain,
        "function": get_function_domain
    }

    if kind not in dispach:
        print(" not Implemented {}".format(kind), file=sys.stderr)
        sys.exit(4)

    return dispach[kind](kind_ele)


def get_int_domain(data):

    (_, rs_ele) = next_ele(data)
    rs = rs_ele['children']

    if len(rs) != 1:
        print(" only len 1 ranges Implemented", file=sys.stderr)
        sys.exit(5)

    rr = rs[0]
    (_, fromto_ele) = next_ele(rr)

    def f(ele):
        if ele['tag'] == 'reference':
            return ele['children'][0]['primitive']
        else:
            return ele['children'][0]['children'][0]['primitive']

    fromto = [  f(ele) for ele in fromto_ele['children'] ]

    bs=[]
    for b in fromto:
        if "string" in b:
            bs.append(Ref(b['string']))
        else:
            bs.append(b['int'])


    return Int(tuple(bs))


def get_function_domain(data):

    top = data['children']

    (_, attrs) =next_ele(top[0])
    atts_names = [ ele['children'][0]['children'][0]['children'][0]['primitive']['string'] for ele in attrs['children'] ]

    # make the attrs into a dict so that they can be passed to __init__ of Func
    as_bools = dict(zip(atts_names, it.repeat(True) ))

    # Assume int domains
    [fromm, to] = [ get_int_domain(next_ele(next_ele(ele)[1])[1]) for ele in top[1:] ]

    f = Func(fromm=[fromm], tos=[to], **as_bools)

    return f


def gather_param_info(essence_file, output_dir):
    """ Get param bounds in json """

    json_path = os.path.join(output_dir, "essence.json")

    sys.stdout.flush()
    sys.stderr.flush()

    subprocess.Popen([
        chain_lib.wrappers("essenceGivensToJson2.sh"), essence_file, json_path, "100"
    ]).communicate()


    with open( json_path ) as f:
        json_in = f.read()

    raw_data = json.loads(json_in)


    param_data = {  data['name']: transform_json_domain_to_domain(data['domain']) for data in raw_data['givens'] }

    return param_data


# from http://docs.python.org/3.3/library/itertools.html
def random_combination_with_replacement(iterable, r):
    "Random selection from itertools.combinations_with_replacement(iterable, r)"
    pool = tuple(iterable)
    n = len(pool)
    indices = sorted(random.randrange(n) for i in range(r))
    return tuple(pool[i] for i in indices)

