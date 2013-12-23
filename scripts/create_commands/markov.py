import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}___{influence_radius}_{influence_radius}_{chain_length}_{radius_as_percentage}";
[ -d \$place ] \
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place \$(models_timeout {limit})";
record_cp \$place/logs/log-{race_no} \\
		../instancegen/mchain/chain_sampling.py cpu {limit} \\
		--models_timeout=\$(models_timeout {limit}) \
		--select_radius={influence_radius} \\
		--influence_radius={influence_radius} \\
		--chain_length={chain_length} \\
		--radius_as_percentage={radius_as_percentage} \\
		--mode=%s \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place
EOF
)"""

	return util.create_commands_py("markov", par_function, data, commons_grouped, place_dir, init_source, num_runs)

