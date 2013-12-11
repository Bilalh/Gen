#compdef chain_main.py ../instancegen/mchain/chain_main.py
_message_next_arg()
{
    argcount=0
    for word in "${words[@][2,-1]}"
    do
        if [[ $word != -* ]] ; then
            ((argcount++))
        fi
    done
    if [[ $argcount -le ${#myargs[@]} ]] ; then
        if [ ${myargs} = "<file>"  ]; then
            _files
        elif [ ${myargs} = "<dir>" ]; then
            _directories
        else;
            _message -r $myargs[$argcount]
        fi
    fi
}

_chain_main.py ()
{
    local context state state_descr line
    typeset -A opt_args

	_arguments -s -S -C \
		':command:->command' \
		'*::options:->options'

	case $state in
		(command)
			local -a subcommands
			subcommands=(
				'iterations'
				'time'
			)
			_values 'chain_main.py' $subcommands
		;;

		(options)
			case $line[1] in
				iterations)
					_chain_main.py-iterations
				;;
				time)
					_chain_main.py-time
				;;
			esac
		;;
	esac

}

_chain_main.py-iterations ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--chain_length=-)--chain_length=-[Length of each chain.]:files:_files' \
		'(--select_radius=-)--select_radius=-[Radius for picking next point.]:files:_files' \
		'(--influence_radius=-)--influence_radius=-[Radius for the acceptance function.]:files:_files' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--model_timeout=-)--model_timeout=-[Timeout in seconds.]:files:_files' \
		'(--working_dir=-)--working_dir=-[Where the essence file is \[default: .\]]:files:_files' \
		'(--seed=-)--seed=-[Random seed to use.]:files:_files' \
		'(--output_dir=-)--output_dir=-[Where to put the results.]:files:_files' \
		'(--mode=-)--mode=-[Conjure mode used \[default: df\].]:files:_files' \
		'(--radius_as_percentage)--radius_as_percentage[Radius setting as in %.]:files:_files' \

    else
        myargs=('<limit>')
        _message_next_arg
    fi
}

_chain_main.py-time ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--chain_length=-)--chain_length=-[Length of each chain.]:files:_files' \
		'(--select_radius=-)--select_radius=-[Radius for picking next point.]:files:_files' \
		'(--influence_radius=-)--influence_radius=-[Radius for the acceptance function.]:files:_files' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--model_timeout=-)--model_timeout=-[Timeout in seconds.]:files:_files' \
		'(--working_dir=-)--working_dir=-[Where the essence file is \[default: .\]]:files:_files' \
		'(--seed=-)--seed=-[Random seed to use.]:files:_files' \
		'(--output_dir=-)--output_dir=-[Where to put the results.]:files:_files' \
		'(--mode=-)--mode=-[Conjure mode used \[default: df\].]:files:_files' \
		'(--radius_as_percentage)--radius_as_percentage[Radius setting as in %.]:files:_files' \

    else
        myargs=('<limit>')
        _message_next_arg
    fi
}


_chain_main.py "$@"
# vim: ft=zsh