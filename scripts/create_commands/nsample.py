import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}___{influence_radius}_{radius_as_percentage}";
mode=%s;
printf ".timeout 5000\\nINSERT OR REPLACE INTO nsample('method', 'essence', 'total_timeout', 'models_timeout', 'races', 'influence_radius', 'radius_as_percentage', 'run_no', 'output_dir') \
	VALUES('nsample', '{essence}', '\$(total_normalised {limit})', '\$(models_timeout_normalised {limit})', '{races}', '{influence_radius}', '\$(to_bool {radius_as_percentage})', '{race_no}', '\$place');" \
		| sqlite3 results/Info.db;
[ -d \$place ] \
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place \$(models_timeout {limit})";
record_cp \$place/logs/log-{race_no} \\
		../instancegen/mchain/nsampling.py cpu {limit} \\
		--models_timeout=\$(models_timeout {limit}) \
		--influence_radius={influence_radius} \\
		--radius_as_percentage={radius_as_percentage} \\
		--mode=\$mode \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place;
\$PARAM_GEN_SCRIPTS/misc/tar_results.sh \$place \$mode;
EOF
)"""

	return util.create_commands_py("nsample", par_function, data, commons_grouped, place_dir, init_source, num_runs)

