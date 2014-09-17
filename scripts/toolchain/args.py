import argparse
import logging

from pathlib import Path

logger = logging.getLogger(__name__)

def do_args():
    parse_args = argparse.ArgumentParser()
    parse_args.add_argument("essence", help='')
    parse_args.add_argument("--outdir", required=True, help='')
    parse_args.add_argument("--timeout", required=True,type=int,
            help='Wall time for everything')
    parse_args.add_argument("--param", help='', default=None)
    parse_args.add_argument("--refine_only", action='store_true')
    args = parse_args.parse_args()

    args.outdir = Path(args.outdir)
    if not args.outdir.exists():
        args.outdir.mkdir(parents=True)
    if not args.param:
        args.param  = args.outdir / "empty.param"
        with  args.param.open("w")  as f:
            f.write("language ESSENCE' 1.0")
            f.write("\n$This Fine is empty")

    else:
        args.param = Path(args.param)

    args.essence = Path(args.essence)
    return args

