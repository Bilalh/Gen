#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Bilal Syed Hussain

from flask import Flask
app = Flask(__name__)


@app.route('/<essence>')
def hello_world(essence):
    return 'Hellos World! %s' % essence

if __name__ == '__main__':
    app.run(debug=True)