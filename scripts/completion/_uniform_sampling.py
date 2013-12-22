#compdef uniform_sampling.py

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

_uniform_sampling.py ()
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
				'json'
				'cpu'
				'iterations'
				'time'
			)
			_values 'uniform_sampling.py' $subcommands
		;;

		(options)
			case $line[1] in
				json)
					_uniform_sampling.py-json
				;;
				cpu)
					_uniform_sampling.py-cpu
				;;
				iterations)
					_uniform_sampling.py-iterations
				;;
				time)
					_uniform_sampling.py-time
				;;
			esac
		;;
	esac

}

_uniform_sampling.py-json ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \

    else
        myargs=('<file>')
        _message_next_arg
    fi
}

_uniform_sampling.py-cpu ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--working_dir=-)--working_dir=-[Where the essence file is \[default: .\]]:files:_files' \
		'(--seed=-)--seed=-[Random seed to use.]:files:_files' \
		'(--output_dir=-)--output_dir=-[Where to put the results.]:files:_files' \
		'(--mode=-)--mode=-[Conjure mode used \[default: df\].]:files:_files' \

    else
        myargs=('<limit>')
        _message_next_arg
    fi
}

_uniform_sampling.py-iterations ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--working_dir=-)--working_dir=-[Where the essence file is \[default: .\]]:files:_files' \
		'(--seed=-)--seed=-[Random seed to use.]:files:_files' \
		'(--output_dir=-)--output_dir=-[Where to put the results.]:files:_files' \
		'(--mode=-)--mode=-[Conjure mode used \[default: df\].]:files:_files' \

    else
        myargs=('<limit>')
        _message_next_arg
    fi
}

_uniform_sampling.py-time ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--working_dir=-)--working_dir=-[Where the essence file is \[default: .\]]:files:_files' \
		'(--seed=-)--seed=-[Random seed to use.]:files:_files' \
		'(--output_dir=-)--output_dir=-[Where to put the results.]:files:_files' \
		'(--mode=-)--mode=-[Conjure mode used \[default: df\].]:files:_files' \

    else
        myargs=('<limit>')
        _message_next_arg
    fi
}


_uniform_sampling.py "$@"
# vim: ft=zsh