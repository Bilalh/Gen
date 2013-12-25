import math
import os
import sqlite3


def calc_models_timeout(common, cores):
	return math.ceil(common['total_time'] / (common['races'] + 1) / cores)


def calc_total_time(common, cores):
	return math.ceil(common['total_time'] / cores)


def create_commands_py(method_name, function_templete, data, commons_grouped, place_dir, init_source, num_runs):
	cur = data['smac']
	cores = data['cores']
	create_method_table(method_name, cur, place_dir)

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
			os.path.dirname(filepath)
		)

		limit_to_models_timeout = {}

		jflag = cores // num_models
		if jflag <=0:
			jflag = 1

		line = 'parallel --joblog %s/races-%03d-%s.joblog --header : --tagstring "R{1}" -j%d $Command ' % (
			os.path.join(place_dir, "results", method_name, name),
			commons[0]['races'],
			'`date +%F_%H-%M_%s`',
			jflag
		)
		line += ' \\\n ::: race_no `seq 1 %d`' % (num_runs)
		line += ' \\\n ::: base_path  %s' % (os.path.join(place_dir, "results", method_name, name))
		line += ' \\\n ::: cores %d' % (jobs)
		line += ' \\\n ::: essence %s \\' % (os.path.basename((os.path.dirname(filepath))))

		def build_dict(common):
			hours = "%3.2f" % (common['total_time'] / 60 / 60)

			settings = {
				"limit": calc_total_time(common, jobs),
				"races": common['races']
			}
			lines.append("# {}h -- {limit}s * {jobs} cores".format(hours, jobs=jobs, **settings))

			_models_timeout = calc_models_timeout(common, jobs)
			limit_to_models_timeout[settings["limit"]] = _models_timeout

			settings.update(cur)
			return settings

		settings_list = [ build_dict(common).items() for common in commons ]

		lookuplines = "\n".join( "		{}) echo {} ;; ".format(k, v) for (k, v) in limit_to_models_timeout.items() )

		models_timeout_func = """
function models_timeout(){
	case $1 in
%s
	esac;
}
export -f models_timeout
		""" % (lookuplines)

		normalised_lookuplines = "\n".join( "		{}) echo {} ;; ".format(k, v * jobs) for (k, v) in limit_to_models_timeout.items() )
		normalised_func = """
function models_timeout_normalised(){
	case $1 in
%s
	esac;
}
export -f models_timeout_normalised
		""" % (normalised_lookuplines)

		normalised_total_lines = "\n".join( "		{}) echo {} ;; ".format(k, k * jobs) for (k, v) in limit_to_models_timeout.items() )
		normalised_total_func = """
function total_normalised(){
	case $1 in
%s
	esac;
}
export -f total_normalised
		""" % (normalised_total_lines)


		lines.append(models_timeout_func)
		lines.append(normalised_func)
		lines.append(normalised_total_func)
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


def create_method_table(method_name, cur, place_dir,):
	# Create a table for this method
	def get_sql_type(val):
		if isinstance(val, int):
			return "INTEGER"
		elif isinstance(val, str):
			return "TEXT"
		elif isinstance(val, float):
			return "FLOAT"
		elif isinstance(val, list):
			return get_sql_type(val[0])
		else:
			print("not sure what to do with %s when mapping to sql", val)
			exit(44)

	val_types = [ (k, get_sql_type(v)) for (k, v) in cur.items() ]

	table_text = create_db_table_query(method_name, *val_types)
	conn = sqlite3.connect(os.path.join(place_dir, "results", "Info.db"))
	conn.execute(table_text)
	conn.commit()


def create_db_table_query(method, *keys):
	templete = """
	CREATE TABLE IF NOT EXISTS  "{method}" (
		{rows},
		quality FLOAT,
		PRIMARY KEY ({keys_joined})
	);
	"""

	pkeys = [("method", "TEXT"), ("essence", "TEXT"),  ("total_timeout", "INTEGER"), ("models_timeout", "INTEGER"), ("races", "INTEGER") ] + list(keys) + [
		("run_no", "INTEGER"), ("output_dir", "TEXT")]
	pnames = [ v[0] for v in pkeys ]
	pjoined = "'" + "', '".join(pnames) + "'"
	pvals = "'{" + "}', '{".join(pnames) + "}'"


	rows = ",\n\t\t".join(  '"{}" {} NOT NULL'.format(name, kind) for (name, kind) in pkeys )

	print('INSERT INTO {method}({keys_joined}) VALUES({vals});'.format(method=method, keys_joined=pjoined, vals=pvals))

	return templete.format(method=method, keys_joined=pjoined, rows=rows)

if __name__ == '__main__':
	print(create_db_table_query("markov", ("s", "TEXT") ))
