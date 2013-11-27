#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

#Plots the chain
import matplotlib
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import random
import json

def iter_many(it, length, num):
    for i in range(0, length, num):
        yield (it[i:i+1 + num])


def plot_chain(current_chain, data_points):
    # get_ipython().magic('matplotlib inline')
    plt.axis("equal")

    fig, ax = plt.subplots()

    ax.scatter(*current_chain[0], c='b')

    if len(data_points) > 0:
        ax.scatter(*list(zip(*data_points)), c='y')

    ax.plot(*list(zip(*current_chain)), c='g')
    ax.scatter(*list(zip(*current_chain[1:])), c='m')
    ax.scatter(*current_chain[0], c='b')
    fig  #show


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

    fig.savefig('temp.pdf', dpi=fig.dpi)


def plot_json_ouput(chain_fp, data_fp, limit=100):
    chain = json.load(open(chain_fp))
    if data_fp:
        data = json.load(open(data_fp))
    else:
        data = []

    plot_chain_3d(chain[0:limit], data[0:limit])


if __name__ == "__main__":
    import sys
    [_, chain_fp, data_fp, limit] = sys.argv
    print([chain_fp, data_fp, limit])
    plot_json_ouput(chain_fp, data_fp, int(limit))
    print("Saved temp.pdf")


