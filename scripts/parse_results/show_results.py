#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from flask import Flask
from flask import render_template
from itertools import groupby
from pprint import pprint, pformat

import parse_results

app = Flask(__name__)


@app.route('/by_option/<option>')
def by_option(option):

	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		essence=None,
		filterer=option
	)
	# pprint(data)


	chart_data = []
	for method_name in parse_results.METHODS:
		for (i, (k, g)) in enumerate(groupby(data[method_name], key=lambda d: d[option] )):
			if len(chart_data) == i:
				d = {
					"title": "{}= {}".format(option, k),
					"subtitle": "Quality for each method grouped by {}".format(option)
					}
				chart_data.append(d)

			chart_data[i][method_name] = list(g)

	pprint(len(chart_data))
	return render_template('chart2.html', chart_data=chart_data )


@app.route('/all')
def show_all_results():
	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		essence=None,
	)

	return render_template('chart.html',
		markov=data['markov'], uniform=data['uniform'], nsample=data['nsample'],
		title='Quality for each configuration of each method',
		subtitle="",
	)


@app.route('/<essence>')
def show_essence(essence):
	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		essence=essence,
	)

	return render_template('chart.html',
		markov=data['markov'], uniform=data['uniform'], nsample=data['nsample'],
		title='Quality for each configuration of each method',
		subtitle=essence,
	)


if __name__ == '__main__':
	app.run(debug=True)

