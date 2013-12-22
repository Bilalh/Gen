import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
record_cp {base_path}/out-{limit}-{races}-{cores}__{race_no}/logs/log-{race_no} \\
		../instancegen/mchain/chain_sampling.py cpu {limit} \\
		--models_timeout={models_timeout} \\
		--select_radius={select_radius} \\
		--influence_radius={influence_radius} \\
		--chain_length={chain_length} \\
		--mode=%s \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir={base_path}/out-{limit}-{races}-{cores}__{race_no} %s
EOF
)"""

	return util.create_commands_py("markov", par_function, data, commons_grouped, place_dir, init_source, num_runs)

