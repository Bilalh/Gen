from lib import chain_lib

from abc import ABCMeta, abstractmethod
from pprint import pprint

import itertools as it
import json
import subprocess
import sys
import os


class Domain(metaclass=ABCMeta):
    """Domain e.g int or function"""
    def __init__(self):
        super(Domain, self).__init__()

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


class DomainInt(Domain):
    """Int domain with (low,high) range"""
    def __init__(self, low_high):
        super(DomainInt, self).__init__()
        self.low_high = low_high

    def random_value(self, selected_vals):
        vs = [ self.resolve(v, selected_vals) for v in self.low_high ]
        return chain_lib.uniform_int(*vs)

    def all_values(self, selected_vals):
        vs = range(*[ self.resolve(v, selected_vals) for v in self.low_high ])
        return vs


class DomainFunc(Domain):
    """Function Domain"""
    def __init__(self, fromm, to, total=False):
        super(DomainFunc, self).__init__()
        self.total = total
        self.fromm = fromm
        self.to = to

    def random_value(self, selected_vals):
        # TODO  assumes total at the momment
        all_from = self.fromm.all_values(selected_vals)
        tos = [ self.to.random_value(selected_vals) for i in range(len(all_from)) ]
        res = dict(zip(all_from, tos))
        return res


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


    return DomainInt(tuple(bs))


def get_function_domain(data):

    top = data['children']

    (_, attrs) =next_ele(top[0])
    atts_names = [ ele['children'][0]['children'][0]['children'][0]['primitive']['string'] for ele in attrs['children'] ]

    # make the attrs into a dict so that they can be passed to __init__ of Func
    as_bools = dict(zip(atts_names, it.repeat(True) ))

    # Assume int domains
    [fromm, to] = [ get_int_domain(next_ele(next_ele(ele)[1])[1]) for ele in top[1:] ]

    f = DomainFunc(fromm=fromm, to=to, **as_bools)

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

