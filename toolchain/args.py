import argparse
import logging
import sys

from pathlib import Path

logger = logging.getLogger(__name__)

def do_args():
    parse_args = argparse.ArgumentParser()
    parse_args.add_argument("essence")
    parse_args.add_argument("--outdir", required=True)
    parse_args.add_argument("--timeout", required=True, type=int,
            help='Real time for everything')
    parse_args.add_argument("--param", help='', default=None)
    parse_args.add_argument("--refine_only", action='store_true')
    parse_args.add_argument("--num_cores", type=int, default=1)
    parse_args.add_argument("--new_conjure", action='store_true',
            help='Use new conjure, must be called conjureNew')
    parse_args.add_argument("--seed", type=int)
    parse_args.add_argument("--refine_all", action='store_true',
            help='Produces all models')
    parse_args.add_argument("--bin_dir", help='Use the specifed directory for binaries (give a full path)')
    parse_args.add_argument("--choices", help='Use the following choices when refining if possible, can not be used with --refine_all')


    args = parse_args.parse_args()

    args.outdir = Path(args.outdir)
    if not args.outdir.exists():
        args.outdir.mkdir(parents=True)
    if not args.param:
        args.param  = args.outdir / "empty.param"
        with args.param.open("w") as f:
            f.write("language Essence 1.3")
            f.write("\n$This file is empty")

    else:
        args.param = Path(args.param)

    if args.refine_all and args.choices:
        print("--refine_all can not be used with --choices", file=sys.stderr)
        sys.exit(1)

    if args.choices:
        args.choices = Path(args.choices)
        if not args.choices.exists():
            print("{} does not exist ".format(args.choices))

    args.essence = Path(args.essence)
    return args
