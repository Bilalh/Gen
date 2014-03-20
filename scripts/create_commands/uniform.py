import util


def create_commands(data, commons_grouped, place_dir, init_source, num_runs):

	par_function = """
Command=$( cat <<EOF
place="{base_path}/out-{limit}-{races}-{cores}__{race_no}_{use_minion}_{pre_generate}";
mode=%s;
[ -d \$place ] \
	&& echo "Not writing to \$place, it exists"
	&& exit;
echo "output_dir is \$place";
record_cp \$place/logs/log-{race_no} \\
		../instancegen/mchain/uniform_sampling.py cpu {limit} \\
		--models_timeout=\$(models_timeout {limit}) \
		--mode=\$mode \\
		--essence=%s \\
		--working_dir=%s \\
		--output_dir=\$place \\
		--generated_dir={working_dir}/generated \\
		--use_minion={use_minion} \\
		--pre_generate={pre_generate} \\
		--info=%s && \\
printf ".timeout 5000\\nINSERT OR REPLACE INTO uniform('method', 'essence', 'total_timeout', 'models_timeout', 'races',  'run_no', 'output_dir', 'use_minion', 'pre_generate') \
	VALUES('uniform', '{essence}', '\$(total_normalised {limit})', '\$(models_timeout_normalised {limit})', '{races}', '{race_no}', '\$place', '\$(to_bool {use_minion})', '\$(to_bool {pre_generate})');" \
		| sqlite3 results/Info.db && \
\$PARAM_GEN_SCRIPTS/misc/tar_results.sh \$place \$mode;
EOF
)"""

	return util.create_commands_py("uniform", par_function, data, commons_grouped, place_dir, init_source, num_runs)

