import math
import os
import util
import random

#Assumes a pcs file is next to the essence
smac_scenario = """
algo = ../../../../instancegen/scripts/wrappers/toolchain_wrapper.py
execdir = results/essences/{essence_dir}
deterministic = 1
run_obj = quality
overall_obj = mean
cutoff_time = {model_timeout}
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
				"model_timeout": util.calc_model_timeout(common, jobs),
				"limit": util.calc_total_time(common, jobs),
				"mode": mode,
				"output_dir": os.path.join("smac-output", extra),
				"seed": random.randint(0, 2 ** 32),
				"log_path": os.path.join("smac-output", extra, "logs", "log-${race_no}"),
				"scenario_path": os.path.join("scenarios", extra0 + ".txt")
			}
			settings.update(cur)


			scenario_text = smac_scenario.format(**settings)
			with open(os.path.join(scenarios_dir, extra0 + ".txt"), "w") as f:
				f.write(scenario_text)

			# print(settings)
			command ="\t" + """
			record_cp {log_path} ../../../instancegen/smac-v2.06.00-master-615/smac\
				--scenario-file {scenario_path}  \
				--rungroup {log_path}   \
				--seed     {seed}
				# {output_dir}
			""".format(**settings).strip().replace("\t", " ")

			lines.append(command)

		lines.append("done")
		return lines



	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}