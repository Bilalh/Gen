#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from flask import Flask
from flask import render_template


app = Flask(__name__)


@app.route('/<essence>')
def hello_world(essence):
    return render_template('chart.html')


if __name__ == '__main__':
    app.run(debug=True)