import math
import os


def calc_models_timeout(common, cores):
	return math.ceil(common['total_time'] / (common['races'] + 1) / cores)


def calc_total_time(common, cores):
	return math.ceil(common['total_time'] / cores)


def create_commands_py(method_name, function_templete, data, commons_grouped, place_dir, init_source, num_runs):
	cur = data[method_name]
	cores = data['cores']

	def func(commons, *, name, filepath, mode, num_models):

		if num_models < cores:
			jobs = num_models
		else:
			jobs = cores

		lines = [
			"export PARAM_GEN_SCRIPTS=`pwd`/../instancegen/scripts/",
			"export NUM_JOBS={}".format(jobs),
			init_source,
			"",
			"### %s ###" % (method_name),
			"#-- {} --#".format(filepath),
		]

		par_function = function_templete % (
			mode,
			filepath,
			os.path.dirname(filepath),
			" ".join([ " --{}".format(k) for (k, v) in cur.items() if v is True ])
		)

		line = 'parallel  --header : --tagstring "R{1}" -j%d $Command ' % (cores / num_models)
		line += ' \\\n ::: race_no `seq 1 %d`' % (num_runs)
		line += ' \\\n ::: base_path  %s' % (os.path.join(place_dir, "results", "markov", name))
		line += ' \\\n ::: cores %d \\' % (jobs)

		def build_dict(common):
			hours = "%3.2f" % (common['total_time'] / 60 / 60)

			settings = {
				"models_timeout": calc_models_timeout(common, jobs),
				"limit": calc_total_time(common, jobs),
				"races": common['races']
			}
			lines.append("# {}h -- {limit}s * {jobs} cores".format(hours, jobs=jobs, **settings))


			settings.update(cur)
			settings = { k: v for (k, v) in settings.items() if v is not True and v is not False }
			return settings

		settings_list = [ build_dict(common).items() for common in commons ]

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

