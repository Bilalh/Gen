import math
import os
import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):
	cur = data['markov']
	cores = data['cores']

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

		for common in commons:
			tu = (int(math.ceil(common['total_time'] / 60 / 60)), common['races'] )
			lines.append("		# {:03}h, {:03} races".format(*tu) )

			extra = "out-{:03}-{:03}__{race_no}".format(*tu, race_no="${race_no}")
			settings = {
				"essence": filepath,
				"essence_dir": os.path.dirname(filepath),
				"model_timeout": util.calc_model_timeout(common, jobs),
				"limit": util.calc_total_time(common, jobs),
				"mode": mode,
				"output_dir": os.path.join(place_dir, "results", "markov", name, extra),
				"log_path": os.path.join(place_dir, "results", "markov", name, extra, "logs", "log-${race_no}")
			}
			settings.update(cur)
			# print(settings)
			command ="\t" + """
			record_cp {log_path} ../instancegen/mchain/chain_main.py time {limit}\
				--model_timeout={model_timeout}\
				--mode={mode} --radius_as_percentage \
				--select_radius={select_radius} --influence_radius={influence_radius} \
				--chain_length={chain_length} \
				--essence={essence} --working_dir={essence_dir} --output_dir={output_dir}
			""".format(**settings).strip().replace("\t", " ")

			lines.append(command)

		lines.append("done")
		return lines

	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}

