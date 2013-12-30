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
				"radius_as_percentage", "influence_radius", "num_points", "run_no", "point_selector"]


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
				if yoption == "discriminating":
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


@app.route('/by_essence/by_option/<option>/yaxis/<yoption>/xaxis/<xoption>')
def by_essence_by_option_by_x(option, yoption, xoption):
	return by_essence_by_option(option, yoption, xoption)


@app.route('/by_essence/by_option/<option>/yaxis/<yoption>')
def by_essence_by_option(option, yoption, xoption=None):

	if xoption:
		xfunc = lambda num, row: row[xoption]
		categories = parse_results.get_values("/Users/bilalh/Desktop/Experiments", xoption)
		cat_dict = dict( (k, i) for (i, k) in enumerate(categories))
		print(categories)

		def add_extra(d):
			d["categories"] = categories
			d['xaxis_title'] = xoption
			return d

		def process(group):
			for g in group:
				g['x'] = cat_dict[g['x']]
			return group

	else:
		xfunc = lambda num, row: num

		def add_extra(d):
			return d

		def process(group):
			return group

	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		filterer=option,
		yfunc=lambda row: row[yoption],
		xfunc=xfunc
	)

	chart_data = {}
	for method_name in parse_results.METHODS:
		for (opt, items) in groupby(data[method_name], key=lambda d: d[option]):
			for (essence_name, g) in groupby(items, key=lambda d: d['essence']):

				if essence_name not in chart_data:
					chart_data[essence_name] = {}

				if opt not in chart_data[essence_name]:
					d = {
						"title": "{}={}  for {}".format(option, opt, essence_name),
						}
					if yoption == "discriminating":
						d['yaxis_title'] = "Number of discriminating params"
						d["subtitle"] = "Number of discriminating params for each method grouped by {}".format(option)
					elif yoption == "quality":
						d['yaxis_title'] = "Quality [0..1] lower is better"
						d["subtitle"] = "Quality for each method grouped by {}".format(option)
						d['yaxis_min'] = 0
						d['yaxis_max'] = 1.5

					chart_data[essence_name][opt] = d
				chart_data[essence_name][opt][method_name] = process(list(g))

	sorted_data = [ ]
	for essence_key in sorted(chart_data):
		for opt_key in sorted(chart_data[essence_key]):
			sorted_data.append(add_extra(chart_data[essence_key][opt_key]))

	return render_template('chart2.html', chart_data=sorted_data )


@app.route('/by_essence/by_option/<option>/by_option/<option2>/yaxis/<yoption>/xaxis/<xoption>')
def by_essence_by_option_by_option_by_x(option, option2, yoption, xoption):
	return by_essence_by_option_by_option(option, option2, yoption, xoption)


@app.route('/by_essence/by_option/<option>/by_option/<option2>/yaxis/<yoption>')
def by_essence_by_option_by_option(option, option2, yoption, xoption=None):

	if xoption:
		xfunc = lambda num, row: row[xoption]
		categories = parse_results.get_values("/Users/bilalh/Desktop/Experiments", xoption)
		cat_dict = dict( (k, i) for (i, k) in enumerate(categories))
		print(categories)

		def add_extra(d):
			d["categories"] = categories
			d['xaxis_title'] = xoption
			return d

		def process(group):
			for g in group:
				g['x'] = cat_dict[g['x']]
			return group

	else:
		xfunc = lambda num, row: num

		def add_extra(d):
			return d

		def process(group):
			return group

	data = parse_results.parse_results(
		"/Users/bilalh/Desktop/Experiments",
		filterer=option,
		yfunc=lambda row: row[yoption],
		xfunc=xfunc
	)


	chart_data = {}
	for method_name in parse_results.METHODS:
		for (opt0, items0) in groupby(data[method_name], key=lambda d: d[option]):
			for (opt, items) in groupby(items0, key=lambda d: d[option2]):
				for (essence_name, g) in groupby(items, key=lambda d: d['essence']):

					if essence_name not in chart_data:
						chart_data[essence_name] = {}

					if opt0 not in chart_data[essence_name]:
						chart_data[essence_name][opt0]= {}

					if opt not in chart_data[essence_name][opt0]:
						d = {
							"title": "{}={}, {}={} for {}".format(option, opt0, option2, opt, essence_name),
							}
						if yoption == "discriminating":
							d['yaxis_title'] = "Number of discriminating params"
							d["subtitle"] = "Number of discriminating params for each method grouped by {}".format(option)
						elif yoption == "quality":
							d['yaxis_title'] = "Quality [0..1] lower is better"
							d["subtitle"] = "Quality for each method grouped by {}".format(option)
							d['yaxis_min'] = 0
							d['yaxis_max'] = 1.5

						chart_data[essence_name][opt0][opt] = d

					chart_data[essence_name][opt0][opt][method_name] = process(list(g))

	sorted_data = [ ]
	for essence_key in sorted(chart_data):
		for opt0_key in sorted(chart_data[essence_key]):
			for opt_key in sorted(chart_data[essence_key][opt0_key]):
				res = chart_data[essence_key][opt0_key][opt_key]
				sorted_data.append(add_extra(res))

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

	if yoption == "discriminating":
		chart_data[0]['yaxis_title'] = "Number of discriminating params"
	elif yoption == "quality":
		chart_data[0]['yaxis_title'] = "Quality [0..1] lower is better"
		chart_data[0]['yaxis_min'] = 0
		chart_data[0]['yaxis_max'] = 1.5

	return render_template('chart2.html', chart_data=chart_data)

if __name__ == '__main__':
	app.run(debug=True)

