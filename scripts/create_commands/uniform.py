import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}";
[ -d \$place ] \
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place";
record_cp \$place/logs/log-{race_no} \\
		../instancegen/mchain/uniform_sampling.py cpu {limit} \\
		--models_timeout=\$(models_timeout {limit}) \
		--mode=%s \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place
EOF
)"""

	return util.create_commands_py("uniform", par_function, data, commons_grouped, place_dir, init_source, num_runs)

