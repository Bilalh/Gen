import random
import math


def in_circle(centre, radius, p):
    """ Return True if if p is the circle """
    square_dist = (centre[0] - p[0]) ** 2 + (centre[1] - p[1]) ** 2
    return square_dist <= radius ** 2


def pick_inside_circle(radius, cx, cy):
    """ Generate a random point inside a circle"""
    r = random.random()
    a = random.random() * 2 * math.pi
    x = math.sqrt(r) * radius * math.cos(a) + cx
    y = math.sqrt(r) * radius * math.sin(a) + cy
    return (x, y)


def pick_on_hypersphere(dim, radius=1):
    """ Returns a point on the surface of a hypersphere"""
    sigma=0.1  # arbitrary  var
    mu = 0     # must have a mean of zero
    xs = [random.gauss(mu, sigma) for x in range(0, dim) ]
    part = radius / ( math.sqrt( sum( x ** 2 for x in xs ) ) )
    return [ x * part for x in xs  ]


def pick_in_hypersphere(dim, radius, centre):
    """ Returns a point inside a hypersphere"""
    xs = pick_on_hypersphere(dim, radius)
    u = random.random()
    return translate( centre, [u ** (1 / dim) * x for x in xs])


def is_in_hypersphere(radius, centre, point):
    """ returns true if the point is in the hypersphere """
    dist_sq = sum((p - q) ** 2 for (p, q) in zip(centre, point))
    return dist_sq <= radius ** 2


def translate(displacement, point):
    """ Moves the point by adding the displacement point """
    return ( tuple((sum(x) for x in zip(displacement, point))))


def translate_points(displacement, points):
    """ Moves each point by adding the displacement point """
    return  ( tuple((sum(x) for x in zip (displacement, p))) for p in points )