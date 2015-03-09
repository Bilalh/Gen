#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from toolchain import run
from command import K
from pprint import pprint
from pathlib import Path

cmd = ["conjureNew",
            "spec.essence",
            "-q",
            "f",
            "-a",
            "c",
            "--output-directory",
            "out",
            "--numbering-start",
            "0",
            "--limit-time",
            "60",
            "--seed",
            "12546561",
            "--log-choices",
            "--log-level=logfollow"
                        ]

vals = dict(choices_json=Path("out") / "choices.json")
result = run.run_conjure_with_choices(60, K.refineRandom, cmd, extra_env={}, vals=vals)

pprint(result)