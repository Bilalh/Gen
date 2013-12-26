#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from flask import Flask
from flask import render_template

import parse_results

app = Flask(__name__)


@app.route('/all')
def show_all_results():
	data = parse_results.parse_results("/Users/bilalh/Desktop/Experiments")
	return render_template('chart.html',
		markov=data['markov'], uniform=data['uniform'], nsample=data['nsample'],
		subtitle="SCP"
	)


if __name__ == '__main__':
	app.run(debug=True)