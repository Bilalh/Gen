#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from flask import Flask
from flask import render_template
from itertools import groupby
from pprint import pprint, pformat

import parse_results

app = Flask(__name__)


@app.route('/')
def start_page():
	options = ["essence", "total_timeout", "models_timeout", "races", "chain_length",
				"radius_as_percentage", "influence_radius", "num_points", "run_no"]


	return render_template('start_page.html', options=options )


@app.route('/by_option/<option>/yaxis/<yoption>')
def by_option(option, yoption):

	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		filterer=option,
		yfunc=lambda row: row[yoption]
	)

	chart_data = {}
	for method_name in parse_results.METHODS:
		for (i, (k, g)) in enumerate(groupby(data[method_name], key=lambda d: d[option] )):
			if k not in chart_data:
				d = {
					"title": "{}={}".format(option, k),
					}
				if yoption == "discriminating_count":
					d['yaxis_title'] = "Number of discriminating params"
					d["subtitle"] = "Number of discriminating params for each method grouped by {}".format(option)
				elif yoption == "quality":
					d['yaxis_title'] = "Quality [0..1] lower is better"
					d["subtitle"] = "Quality for each method grouped by {}".format(option)
					d['yaxis_min'] = 0
					d['yaxis_max'] = 1.5

				chart_data[k] = d
			chart_data[k][method_name] = list(g)

	sorted_data = [ chart_data[k] for k in sorted(chart_data)]
	return render_template('chart2.html', chart_data=sorted_data )


@app.route('/all/yaxis/<yoption>')
def show_all_results(yoption):
	data = parse_results.parse_results("/Users/bilalh/Desktop/Experiments", yfunc=lambda row: row[yoption])
	pprint(data['uniform'])

	chart_data = [{
		"title": 'Quality for each configuration of each method',
		"subtitle": "",
		"markov": data['markov'],
		"uniform": data['uniform'],
		"nsample": data['nsample'],
		"smac": data['smac'],
		"ksample": data['ksample'],
	}]

	if yoption == "discriminating_count":
		chart_data[0]['yaxis_title'] = "Number of discriminating params"
	elif yoption == "quality":
		chart_data[0]['yaxis_title'] = "Quality [0..1] lower is better"
		chart_data[0]['yaxis_min'] = 0
		chart_data[0]['yaxis_max'] = 1.5

	return render_template('chart2.html', chart_data=chart_data)

if __name__ == '__main__':
	app.run(debug=True)

