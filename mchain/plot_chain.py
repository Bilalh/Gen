#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

#Plots the chain
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import random
import json

from lib.ncube import pick_inside


def iter_many(it, length, num):
    for i in range(0, length, num):
        yield (it[i:i+1 + num])


def plot_chain_2d(current_chain, data_points):
    # get_ipython().magic('matplotlib inline')
    plt.axis("equal")

    fig, ax = plt.subplots()


    ax.scatter(*current_chain[0], c='b')

    if len(data_points) > 0:
        ax.scatter(*list(zip(*data_points)), c='y')

    ax.plot(*list(zip(*current_chain)), c='g')
    ax.scatter(*list(zip(*current_chain[1:])), c='m')
    ax.scatter(*current_chain[0], c='b')

    fig.savefig('temp_2d.pdf', dpi=fig.dpi)


def plot_chain_3d(current_chain, data_points):
    fig = plt.figure()
    fig.clf()

    ax = fig.add_subplot(111, projection='3d')

    ax.scatter(*current_chain[0], c='b')

    if len(data_points) > 0:
        ax.scatter(*list(zip(*data_points)), c='y')

    ax.scatter(*list(zip(*current_chain[1:])), c='m')

    for ps in iter_many(current_chain, len(current_chain), 10):
        ax.plot(*list(zip(*ps)), c=(random.random(), random.random(), random.random()) )

    ax.scatter(*current_chain[0], c='b')

    fig.savefig('temp_3d.pdf', dpi=fig.dpi)


def plot_json_ouput(chain_fp, data_fp, limit=100):
    chain = json.load(open(chain_fp))
    if data_fp:
        data = json.load(open(data_fp))
    else:
        data = []

    print(chain[0])
    if len(chain[0]) == 3:
        plot_chain_3d(chain[0:limit], data[0:limit])
    else:
        plot_chain_2d(chain[0:limit], data[0:limit])


def test_ncube():
    data_points = [pick_inside_ncube2((5, 3), (50, 25), ) for i in range(0, 50)]
    fig, ax = plt.subplots()
    ax.scatter(*list(zip(*data_points)), c='b')
    print(data_points)


    fig = plt.figure()
    fig.clf()

    data_points = [pick_inside_ncube2((5, 3, 5) , (50, 25, 50), ) for i in range(0, 50)]
    print(data_points)

    ax = fig.add_subplot(111, projection='3d')
    ax.scatter(*list(zip(*data_points)), c='b')



if __name__ == "__main__":
    import sys
    [_, chain_fp, data_fp, limit] = sys.argv
    print([chain_fp, data_fp, limit])
    plot_json_ouput(chain_fp, data_fp, int(limit))
    print("fin")
