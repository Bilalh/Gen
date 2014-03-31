from lib import chain_lib
from lib import instances
from lib.instances import jmatch

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
import itertools
import operator
import functools

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


    def all_values(self, selected_vals):
        raise NotImplemented("All values not done yet")

    @abstractmethod
    def within_radius_dom(self, selected_vals, instance, radius):
        pass


    @abstractmethod
    def reconstruct_for_smac(self, selected_vals, kv):
        """ SMAC only works with ints, no nesting or matrixes or sets
            recreate the essence params from the flattened input
        """
        pass


class TypeInt(Domain):
    """TypeInt domain with allowed domain (low,high) inclusive """
    def __init__(self, low_high):
        super(TypeInt, self).__init__()
        self.low_high = low_high

    def random_value(self, selected_vals):

        d_low = chain_lib.uniform_int(*self.low_high)
        d_high = chain_lib.uniform_int(d_low, self.low_high[1])

        pretty = "domain int({}..{})".format(d_low, d_high)
        safe = "typeInt__{}_{}__".format(d_low, d_high)

        return instances.TypeInt(point=(d_low, d_low),  pretty=pretty, safe=safe )

    def within_radius_dom(self, selected_vals, instance, radius):
        raise NotImplementedError()


    def reconstruct_for_smac(self, selected_vals, kv):
        assert len(kv) == 2
        [(name, d_low, _, _), (_, d_high, _, _)] = sorted(kv)
        logger.info("{} low:{} high:{}".format(name, d_low, d_high))
        if d_low > d_high:
            raise InvaildValueException("{} low:{} high:{}".format(name, d_low, d_high))

        pretty = "domain int({}..{})".format(d_low, d_high)
        safe = "typeInt__{}_{}__".format(d_low, d_high)

        return instances.TypeInt(point=(d_low, d_low),  pretty=pretty, safe=safe)



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


    def resolve_dom(self, selected_vals):
        vs = [ self.resolve(v, selected_vals).point for v in self.low_high ]
        if vs[0] > vs[1] or vs[1] < vs[0]:
            raise NoValuesInDomainException("low_high: ", vs)

        return Int(vs)

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

    def all_values_(self, selected_vals):
        a=[]
        for u in self.all_values(selected_vals):
            pretty = "{}".format(u)
            a.append(instances.Int(point=u,  pretty=pretty, safe=pretty ))

        return a

    def reconstruct_for_smac(self, selected_vals, kv):
        [(_, val, _, _)] = kv
        return self.from_int(val)

    @staticmethod
    def from_int(val):
        pretty = "{}".format(val)
        return instances.Int(point=val, pretty=pretty, safe=pretty)


class FuncMinion(Domain):
    """ Any kind of Function  """
    def __init__(self, fromm, tos,):
        super(FuncMinion, self).__init__()
        self.fromm = fromm
        self.tos = tos

    def random_value(self, selected_vals):
        raise NotImplementedError()

    def within_radius_dom(self, selected_vals, instance, radius):
        raise NotImplementedError()

    def instance_from_dict_total_func_int_int(self, res):
        kv = [ "{} --> {}".format(k, v.pretty) for (k, v) in sorted(res.items()) ]
        pretty = "function( {} )".format( ", ".join(kv) )

        kv_safe = [ "{}_{}".format(k, v.safe) for (k, v) in sorted(res.items()) ]
        safe = "F__{}__".format( ",".join(kv_safe) )

        return instances.Func(point=res, pretty=pretty, safe=safe)


    def instance_from_dict(self, res):
        kv = [ "{} --> {}".format(k.pretty, v.pretty) for (k, v) in sorted(res.items()) ]
        pretty = "function( {} )".format( ", ".join(kv) )

        kv_safe = [ "{}_{}".format(k.safe, v.safe) for (k, v) in sorted(res.items()) ]
        safe = "F__{}__".format( ",".join(kv_safe) )

        return instances.Func(point=res, pretty=pretty, safe=safe)

    def reconstruct_for_smac(self, selected_vals, kv):

        froms = []
        tos = []
        tags = { kind for (_, _, kind, _) in kv  }
        # sort by var num then var type

        if "FT" in tags:
            s_kv = sorted(kv, key=lambda k: (k[3], k[2]) )
            for _, val, kind, index in s_kv:
                ival = Int.from_int(val)
                if kind == 'FT':
                    froms.append(index)
                    tos.append(ival)
                else:
                    raise ValueError("Invaild tag " + kind)

            from_doms = [ from_dom.resolve_dom(selected_vals) for from_dom in [self.fromm] ]
            # Assume total from ints
            assert len(from_doms) == 1
            num_elems = len(range(from_doms[0].low_high[0], from_doms[0].low_high[1] + 1))

            elems_needed = list(zip(froms, tos))[0:num_elems]

            return self.instance_from_dict_total_func_int_int( dict(elems_needed) )
        elif "FT_T1" in tags:
            s_kv = sorted(kv, key=lambda k: (int(k[3]), k[2]) )

            from_doms = [ from_dom.resolve_dom( selected_vals) for from_dom in [self.fromm] ]
            from_size = sum(  prod(len(i.all_values(selected_vals)) for i in f) for f in from_doms )

            assert len(s_kv) % 3 == 0
            for (_, v1, k1, i1), (_, v2, k2, i2), (_, v3, k3, i3), in iter_many(s_kv, len(s_kv), 3):
                assert k1 < k2 < k3
                assert i1 == i2 == i3

                vv = [ Int.from_int(int(v1)), Int.from_int(int(v2)) ]
                t = instances.Tuple.from_values(vv)
                froms.append(t)
                tos.append(Int.from_int(v3))

            elems_needed = list(zip(froms, tos))[0:from_size]
            return self.instance_from_dict( dict(elems_needed) )



def prod(iterable):
    return functools.reduce(operator.mul, iterable, 1)


def iter_many(it, length, num):
    for i in range(0, length, num):
        yield (it[i:i + num])


class Tuple(Domain):
    """Tuple"""
    def __init__(self, elems):
        super(Tuple, self).__init__()
        self.elems = elems

    def random_value(self, selected_vals):
        raise NotImplementedError()

    def within_radius_dom(self, selected_vals, instance, radius):
        raise NotImplementedError()

    def reconstruct_for_smac(self, selected_vals, kv):
        raise NotImplementedError()


    def resolve_dom(self, selected_vals):
        def f(v):
            if isinstance(v, Ref):
                return v.resolve(selected_vals)
            else:
                return v.resolve_dom(selected_vals)

        return [ f(v) for v in self.elems ]

    def size(self):
        return sum(  len(e.all_values()) for e in self.elems )


# FIXME  use the form  1:instance at the moment
class FuncTotalIntToInt(Domain):
    """Total Function Domain"""
    def __init__(self, fromm, tos, total=False):
        super(FuncTotalIntToInt, self).__init__()
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

        #merge
    def instance_from_dict(self, res):
        kv = [ "{} --> {}".format(k, v.pretty) for (k, v) in sorted(res.items()) ]
        pretty = "function( {} )".format( ", ".join(kv) )

        kv_safe = [ "{}_{}".format(k, v.safe) for (k, v) in sorted(res.items()) ]
        safe = "F__{}__".format( ",".join(kv_safe) )

        return instances.Func(point=res, pretty=pretty, safe=safe)


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

    def reconstruct_for_smac(self, selected_vals, kv):
        raise NotImplementedError("use other Func")


class Set(Domain):
    """Domain for set, which maybe nested"""
    def __init__(self, inner_dom, *, size=None):
        super(Set, self).__init__()
        self.inner_dom = inner_dom
        self.size = size

    def random_value(self, selected_vals):
        raise NotImplementedError()

    def within_radius_dom(self, selected_vals, instance, radius):
        raise NotImplementedError()

    def reconstruct_for_smac(self, selected_vals, kv):
        raise NotImplementedError()


class Rel(Domain):
    """Domain for relation, which maybe nested"""
    def __init__(self, inner_dom, *, size=None):
        super(Rel, self).__init__()
        self.inner_dom = inner_dom
        self.size = size

    def random_value(self, selected_vals):
        raise NotImplementedError()

    def within_radius_dom(self, selected_vals, instance, radius):
        raise NotImplementedError()

    def reconstruct_for_smac(self, selected_vals, kv):
        raise NotImplementedError()


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



class InvaildValueException(Exception):
    pass


class NoValuesInDomainException(Exception):
    pass


class UninitialisedVal(Exception):
    """Thrown when a reference is unresolved"""
    pass


Func = None


def setup_domain(use_minion):
    global Func
    if use_minion:
        Func = FuncMinion
    else:
        Func = FuncTotalIntToInt


def next_ele(ele):
    next_ele = ele['children'][0]
    return (next_ele['tag'], next_ele)


def transform_json_domain_to_domain(data):
    dom = data[0]


    assert dom['tag'] == 'domain'

    (kind, kind_ele) = next_ele(dom)

    return domain_dispacher(kind, kind_ele)


def domain_dispacher(kind, kind_ele):
    dispach = {
        "int": get_int_domain,
        "function": get_function_domain,
        "typeInt": get_typeInt_domain,
        "set": get_set_domain,
        "tuple": get_tuple_domain,
        "relation": get_relation_domain
    }

    if kind not in dispach:
        raise NotImplementedError(kind)

    return dispach[kind](kind_ele)


def get_relation_domain(data):
    rel = jmatch(data, 'relation')

    (_, attrs) =next_ele(rel[0])
    atts_names = [ ele['children'][0]['children'][0]['children'][0]['primitive']['string'] for ele in attrs['children'] ]

    doms_data = jmatch(rel[1],    'inners', 'domain')

    inner_dom = [ domain_dispacher(dom['tag'], dom) for dom in doms_data ]

    # FIXME relation attrs
    assert len(atts_names) == 0
    return Rel(inner_dom)


def get_tuple_domain(data):

    doms_data = jmatch(data, 'tuple', 'inners', 'domain')

    inner_doms = [ domain_dispacher(dom['tag'], dom) for dom in doms_data ]
    return Tuple(inner_doms)


def get_typeInt_domain(data):
    # FIXME how to specify more data
    return TypeInt( (1, 5))


def get_int_domain(data):

    (_, rs_ele) = next_ele(data)
    rs = rs_ele['children']

    if len(rs) != 1:
        raise NotImplementedError('only len 1 ranges Implemented')

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

    if Func == FuncTotalIntToInt:
        # Assume int domains
        [fromm, to] = [ get_int_domain(next_ele(next_ele(ele)[1])[1]) for ele in top[1:] ]

        f = FuncTotalIntToInt(fromm=[fromm], tos=[to], **as_bools)
    else:

        def handle(ele):
            kind_ele= jmatch(ele, "*", "domain")
            assert len(kind_ele) == 1
            kind_ele = kind_ele[0]
            return domain_dispacher(kind_ele['tag'], kind_ele)

        [fromm, tos] = [  handle(ele) for ele in top[1:] ]
        f = FuncMinion(fromm, tos)

    return f


def handle_set_attribute(attr_dom):
    pair = jmatch(attr_dom, 'attribute', 'nameValue')
    assert len(pair) == 2

    value_index = 1

    try:
        ref_dom =jmatch(pair[0], 'name', 'reference')
    except instances.NotTagFoundExeception:
        logger.info("attr[0] is not name")
        ref_dom =jmatch(pair[1], 'name', 'reference')
        value_index =0

    name = ref_dom[0]['primitive']['string']

    value_dom = jmatch(pair[value_index], "value", "value", "literal" )
    value = value_dom[0]['primitive']['int']

    return (name, value)


def get_set_domain(data):
    top = data['children']

    assert len(top) == 2, "set has attr and child doms"

    dom_index = 1
    try:
        attrs_dom = jmatch(top[0], 'attributes', 'attrCollection' )
    except instances.NotTagFoundExeception:
        logger.info("top[0] is not the attrs")
        attrs_dom = jmatch(top[1], 'attributes', 'attrCollection' )
        dom_index = 0

    attrs = dict( handle_set_attribute(atrr) for atrr in attrs_dom )

    dom_top = top[dom_index]

    dom_outer = jmatch(dom_top, "inner", "domain")
    assert len(dom_outer) == 1

    inner_kind = dom_outer[0]['tag']
    inner_ele = dom_outer[0]

    inner_dom = domain_dispacher(inner_kind, inner_ele)

    return Set(inner_dom, **attrs)


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

    pprint(param_data)

    return param_data


# from http://docs.python.org/3.3/library/itertools.html
def random_combination_with_replacement(iterable, r):
    "Random selection from itertools.combinations_with_replacement(iterable, r)"
    pool = tuple(iterable)
    n = len(pool)
    indices = sorted(random.randrange(n) for i in range(r))
    return tuple(pool[i] for i in indices)

