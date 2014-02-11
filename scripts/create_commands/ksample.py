import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}___{influence_radius}_{radius_as_percentage}_{num_points}_{point_selector}";
mode=%s;
[ -d \$place ] \
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place \$(models_timeout {limit})";
record_cp \$place/logs/log-{race_no} \\
		../instancegen/mchain/ksampling.py cpu {limit} \\
		--models_timeout=\$(models_timeout {limit}) \
		--influence_radius={influence_radius} \\
		--radius_as_percentage={radius_as_percentage} \\
		--num_points={num_points} \\
		--point_selector={point_selector}\\
		--mode=\$mode \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place \\
		--info=%s && \
printf ".timeout 5000\\nINSERT OR REPLACE INTO ksample('method', 'essence', 'total_timeout', 'models_timeout', 'races', 'num_points', 'point_selector', 'influence_radius', 'radius_as_percentage', 'run_no', 'output_dir') \
	VALUES('ksample', '{essence}', '\$(total_normalised {limit})', '\$(models_timeout_normalised {limit})', '{races}', '{num_points}', '{point_selector}', '{influence_radius}', '\$(to_bool {radius_as_percentage})', '{race_no}', '\$place');" \
		| sqlite3 results/Info.db && \
\$PARAM_GEN_SCRIPTS/misc/tar_results.sh \$place \$mode;
EOF
)"""

	return util.create_commands_py("ksample", par_function, data, commons_grouped, place_dir, init_source, num_runs)

