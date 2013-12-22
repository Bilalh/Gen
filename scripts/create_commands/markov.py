import math
import os
import util
from pprint import pprint


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):
	cur = data['markov']
	cores = data['cores']

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
		]

		par_function = """
Command=$( cat <<EOF
record_cp {base_path}/out-{limit}-{races}-{cores}__{race_no}/logs/log-{race_no} \\
		../instancegen/mchain/chain_sampling.py time {limit} \\
		--models_timeout={models_timeout} \\
		--mode=%s \\
		--select_radius={select_radius} \\
		--influence_radius={influence_radius} \\
		--chain_length={chain_length} \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir={base_path}/out-{limit}-{races}-{cores}__{race_no} %s

EOF
)""" % (
			mode,
			filepath,
			os.path.dirname(filepath),
			" ".join([ " --{}".format(k) for (k, v) in cur.items() if v is True ])
		)

		line = 'parallel  --header : --dry-run --tagstring "run-{1}" -j%d $Command ' % (cores / num_models)
		line += ' \\\n ::: race_no `seq 1 %d`' % (num_runs)
		line += ' \\\n ::: base_path  %s' % (os.path.join(place_dir, "results", "markov", name))
		line += ' \\\n ::: cores %d \\' % (jobs)

		def build_dict(common):
			hours = int(math.ceil(common['total_time'] / 60 / 60))

			settings = {
				"models_timeout": util.calc_models_timeout(common, jobs),
				"limit": util.calc_total_time(common, jobs),
				"races": common['races']
			}
			lines.append("# {:03}h {limit}s * {jobs} cores".format(hours, jobs=jobs, **settings))


			settings.update(cur)
			settings = { k: v for (k, v) in settings.items() if v is not True and v is not False }
			return settings

		settings_list = [ build_dict(common).items() for common in commons ]

		lines.append(par_function)
		lines.append(line)

		def convert(d):
			name = d[0][0]
			return " ".join( str(v) for v in ["    :::", name] + sorted(list(set([ kv[1] for kv in d]))) )


		arr = [ convert(items) for items in zip(*settings_list) ]
		parallel_args = " \\\n".join(arr)
		lines.append(parallel_args)

		# [print(l) for l in lines]

		return lines

	return { kv['name']: {num: func(commons, **kv)
		for (num, commons) in commons_grouped.items()}
		for kv in data['essences']
		}

