#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain
import random
import math

import matplotlib.pyplot as plt

def make_point2( cx,cy, radius ):
    """ Generate a random point on a unit disk"""
    r = random.random()
    a = random.random()* 2 * math.pi
    x = math.sqrt(r) * radius * math.cos(a) + cx
    y = math.sqrt(r) * radius * math.sin(a) + cy
    return (x,y)
    

vals = [ make_point2(6,-2,2) for i in range(1,10000)]
[xs,ys] = list(zip(*vals))


fig= plt.plot()
plt.scatter(xs, ys)
plt.show()
