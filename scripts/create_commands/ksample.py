import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}___{influence_radius}_{radius_as_percentage}_{num_points}";
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
		--mode=%s \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place
EOF
)"""

	return util.create_commands_py("ksample", par_function, data, commons_grouped, place_dir, init_source, num_runs)

