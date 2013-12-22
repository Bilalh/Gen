import math
import os
import util
import random

#Assumes a pcs file is next to the essence
smac_scenario = """
algo = ../../../../instancegen/scripts/wrappers/toolchain_wrapper.py
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
			"### markov ###",
			init_source,
			"#-- {} --#".format(filepath),
			"for race_no in {1..%d}; do" % (num_runs)
		]

		essence_base = os.path.splitext(os.path.basename(filepath))[0]
		scenarios_dir = os.path.join(place_dir, "results", "smac", essence_base, "scenarios")
		os.makedirs(scenarios_dir, exist_ok=True)

		for common in commons:
			tu = (int(math.ceil(common['total_time'] / 60 / 60)), common['races'] )
			lines.append("		# {:03}h, {:03} races".format(*tu) )

			extra0 = "out-{:03}-{:03}".format(*tu)
			extra = "{}__{race_no}".format(extra0, race_no="${race_no}")

			settings = {
				"essence": filepath,
				"essence_dir": os.path.dirname(filepath),
				"essence_base": essence_base,
				"models_timeout": util.calc_models_timeout(common, jobs),
				"limit": util.calc_total_time(common, jobs),
				"mode": mode,
				"output_dir": os.path.join("smac-output", extra),
				"seed": random.randint(0, 2 ** 12),
				"log_path": os.path.join("smac-output", extra, "logs", "log-${race_no}"),
				"group_path": os.path.join(extra, "smac-output"),
				"scenario_path": os.path.join("scenarios", extra0 + ".txt")
			}
			settings.update(cur)


			scenario_text = smac_scenario.format(**settings)
			with open(os.path.join(scenarios_dir, extra0 + ".txt"), "w") as f:
				f.write(scenario_text)

			# print(settings)
			command ="\t" + """
			record_cp {log_path} ../../../../instancegen/smac-v2.06.00-master-615/smac\
				--scenario-file {scenario_path}  \
				--rungroup {group_path}   \
				--seed     {seed}

			""".format(**settings).strip().replace("\t", " ")

			lines.append("pushd results/smac/{essence_base}".format(**settings))
			lines.append("export OUT_BASE_DIR=`pwd`/{output_dir}".format(**settings))
			lines.append(command)
			lines.append("popd")

			lines.append("$PARAM_GEN_SCRIPTS/db/parse_smac_output.py --essence={essence} --output_dir=results/smac/{essence_base}/{output_dir}"
				.format(**settings))
			lines.append("")

		lines.append("done")
		return lines



	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}