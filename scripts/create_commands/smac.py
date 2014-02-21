import math
import os
import util
import random

#Assumes a pcs file is next to the essence
smac_scenario = """
algo = ./../../../../instancegen/mchain/smac_process.py {essence_base}.essence info.json {num_models} {mode}
execdir = ../../essences/{essence_base}
deterministic = 1
run_obj = quality
overall_obj = mean
cutoff_time = {models_timeout}
cutoff_length = max
tunerTimeout = {limit}
paramfile = params.pcs
instance_file = ../../smac/zinstances-empty
test_instance_file = ../../smac/zinstances-empty
"""


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):
	cur = data['smac']
	cores = data['cores']
	util.create_method_table("smac", cur, place_dir)

	os.makedirs( os.path.join(place_dir, "results", "smac"), exist_ok=True)
	with open(os.path.join(place_dir, "results", "smac", "zinstances-empty"), "w") as f:
		f.write("empty")

	def func(commons, *, name, filepath, mode, num_models):
		if num_models < cores:
			jobs = num_models
		else:
			jobs = cores

		lines = [
			"export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
			"export NUM_JOBS={}".format(jobs),
			init_source,
			""
			"### smac ###",
			"#-- {} --#".format(filepath),
		]

		par_function="""
		Command=$( cat <<EOF
place="{base_path}/smac-output/out-{limit}-{races}-{cores}__{race_no}";
prefix="out-{limit}-{races}-{cores}__{race_no}";
[ -d \$place ] \\
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place";
pushd results/smac/{essence_base};
export OUT_BASE_DIR=\$PWD/smac-output/\$prefix;
record_cp smac-output/\$prefix/logs/log-{race_no} ../../../../instancegen/smac-v2.06.00-master-615/smac\\
	--scenario-file scenarios/out-{limit}-{races}-{cores}.txt  \\
	--rungroup \$prefix/smac-output    \\
	--seed     \$(seed_for_limit {limit} {race_no});
popd;
printf ".timeout 5000\\nINSERT OR REPLACE INTO smac('method', 'essence', 'total_timeout', 'models_timeout', 'races', 'run_no', 'output_dir') \\
	VALUES('smac', '{essence_base}', '\$(total_normalised {limit})', '\$(models_timeout_normalised {limit})', '{races}', '{race_no}', '\$place');" \\
		| sqlite3 results/Info.db && \\
\$PARAM_GEN_SCRIPTS/misc/tar_results.sh \$place {mode};
EOF
)
"""

		essence_base = os.path.splitext(os.path.basename(filepath))[0]
		scenarios_dir = os.path.join(place_dir, "results", "smac", essence_base, "scenarios")
		os.makedirs(scenarios_dir, exist_ok=True)

		limit_to_models_timeout = {}

		jflag = cores // num_models
		if jflag <=0:
			jflag = 1

		line = 'parallel --joblog %s/races-%03d-%s.joblog --header : --tagstring "R{1}" -j%d $Command ' % (
			os.path.join(place_dir, "results", "smac", name),
			commons[0]['races'],
			'`date +%F_%H-%M_%s`',
			jflag
		)

		line += ' \\\n ::: race_no `seq 1 %d`' % (num_runs)
		line += ' \\\n ::: base_path  %s' % (os.path.join(place_dir, "results", "smac", name))
		line += ' \\\n ::: cores %d' % (jobs)
		line += ' \\\n ::: essence_base %s' % (essence_base)
		line += ' \\\n ::: essence_path %s' % (filepath)
		line += ' \\\n ::: mode %s \\' % (mode)



		def build_dict(common):
			hours = "%3.2f" % (common['total_time'] / 60 / 60)

			settings = {
				"limit": util.calc_total_time(common, jobs),
				"races": common['races'],
			}
			lines.append("# {}h -- {limit}s on {jobs} cores".format(hours, jobs=jobs, **settings))

			_models_timeout = util.calc_models_timeout(common, jobs)
			limit_to_models_timeout[settings["limit"]] = _models_timeout

			settings.update(cur)

			scenario_settings = {
				"essence": filepath,
				"essence_dir": os.path.dirname(filepath),
				"essence_base": essence_base,
				"models_timeout": util.calc_models_timeout(common, jobs),
				"mode": mode,
				"num_models": num_models
			}
			scenario_settings.update(settings)
			scenario_settings.update(cur)

			scenario_text = smac_scenario.format(**scenario_settings)
			scenarios_name = "out-{limit}-{races}-{cores}.txt".format(cores=jobs, **settings)


			with open(os.path.join(scenarios_dir, scenarios_name), "w") as f:
				f.write(scenario_text)


			return settings

		settings_list = [ build_dict(common).items() for common in commons ]

		normalised_lookuplines = "\n".join( "		{}) echo {} ;; ".format(k, v) for (k, v) in limit_to_models_timeout.items() )
		normalised_func = """
function models_timeout_normalised(){
	case $1 in
%s
	esac;
}
export -f models_timeout_normalised
		""" % (normalised_lookuplines)

		normalised_total_lines = "\n".join( "		{}) echo {} ;; ".format(k, k) for (k, v) in limit_to_models_timeout.items() )
		normalised_total_func = """
function total_normalised(){
	case $1 in
%s
	esac;
}
export -f total_normalised
		""" % (normalised_total_lines)

		seed_lines = "\n".join( "		{}) echo $(({}+r)) ;; ".format(k, math.ceil(random.uniform(- 1, 2 ** 16))) for (k, v) in limit_to_models_timeout.items() )
		seed_func = """
function seed_for_limit(){
	r=$2
	case $1 in
%s
	esac;
}
export -f seed_for_limit
		""" % (seed_lines)

		lines.append(normalised_func)
		lines.append(normalised_total_func)
		lines.append(seed_func)
		lines.append(par_function)
		lines.append(line)

		def convert(d):
			name = d[0][0]
			part =[ kv[1] for kv in d]
			if isinstance(part[0], list):
				part = part[0]
			return " ".join( str(v) for v in ["    :::", name] + sorted(set(part)) )

		arr = [ convert(items) for items in zip(*settings_list) ]
		parallel_args = " \\\n".join(arr)
		lines.append(parallel_args)

		# [print(l) for l in lines]

		return lines


	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}