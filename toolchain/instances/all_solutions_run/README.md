Run `create_all_solutions.sh` to create all solutions

Then run `split_results.sh` to split the solutions files into small parts for use with `gen instance*`

if don't want the script to use all your cores do the following:

	export NUM_JOBS=<int>

example to these solutions:

	gen instance-undirected <essence> -p30 -i3 ---mode df --generated-solutions <THIS DIR>