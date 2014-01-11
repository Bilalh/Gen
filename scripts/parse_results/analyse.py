# -*- coding: utf-8 -*-

from contextlib import contextmanager
from pathlib import Path
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import itertools as it


@contextmanager
def max_rows(num_rows):
    old_max = pd.get_option("display.max_rows")
    pd.set_option("display.max_rows",num_rows)
    yield
    pd.set_option("display.max_rows",old_max)


@contextmanager
def max_cols(num_cols):
    old_max = pd.get_option("display.max_columns")
    pd.set_option("display.max_columns",num_cols)
    yield
    pd.set_option("display.max_columns",old_max)


@contextmanager
def max_rc(num_rows, num_cols):
    old_cols = pd.get_option("display.max_columns")
    old_rows = pd.get_option("display.max_rows")
    pd.set_option("display.max_columns",num_cols)
    pd.set_option("display.max_rows",num_rows)
    yield
    pd.set_option("display.max_rows",old_rows)
    pd.set_option("display.max_columns",old_cols)


def put_legend_below(ax):
    box = ax.get_position()
    ax.set_position([box.x0, box.y0 + box.height * 0.1,
                 box.width, box.height * 0.9])
    ax.legend(loc='upper center', bbox_to_anchor=(0.5, -0.05),
          fancybox=True, shadow=True, ncol=5)


def put_legend_on_right_side(ax):
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])

    # Put a legend to the right of the current axis
    ax.legend(loc='center left', bbox_to_anchor=(1, 0.5))


def load_data_frame():
    base_str = Path("/Users/bilalh/Desktop/Experiments")
    df = pd.read_csv(str(base_str / "all.csv"))

    df['resulting_models'] = df['num_models'] * df['quality']
    df['resulting_models_filled'] = df['resulting_models'].fillna(df['num_models'])
    df['total_timeout_h'] = df['total_timeout'] /60/ 60
    df['output_dir'] = df['output_dir'].apply(func=lambda p:  base_str / p  )

    return df

def load_data_frame_param_eprime_info():
    base_str = Path("/Users/bilalh/Desktop/Experiments")
    df = pd.read_csv(str(base_str / "extra_data" / "param_eprime_info.csv"))
    df["eprimes"].fillna("", inplace=True)
    return df


def make_style_mapping(names, styles):
    """ Returns a dict of all subsequences of names --> styles """
    zipped = list(zip(names, styles,))

    paired = it.chain( *(it.combinations(zipped,i + 1) for i in range(len(names))) )
    mapping = dict( zip(*pairs) for pairs in paired )
    return mapping
