#compdef nsampling.py

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

_nsampling.py ()
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
			_values 'nsampling.py' $subcommands
		;;

		(options)
			case $line[1] in
				json)
					_nsampling.py-json
				;;
				cpu)
					_nsampling.py-cpu
				;;
				iterations)
					_nsampling.py-iterations
				;;
				time)
					_nsampling.py-time
				;;
			esac
		;;
	esac

}

_nsampling.py-json ()
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

_nsampling.py-cpu ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--influence_radius=-)--influence_radius=-[Radius for the acceptance function.]:files:_files' \
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

_nsampling.py-iterations ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--influence_radius=-)--influence_radius=-[Radius for the acceptance function.]:files:_files' \
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

_nsampling.py-time ()
{
    local context state state_descr line
    typeset -A opt_args

    if [[ $words[$CURRENT] == -* ]] ; then
    	_arguments -C \
    	':command:->command' \
		'(--essence=-)--essence=-[Essence file.]:files:_files' \
		'(--models_timeout=-)--models_timeout=-[Timeout in seconds.]:files:_files' \
		'(--influence_radius=-)--influence_radius=-[Radius for the acceptance function.]:files:_files' \
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


_nsampling.py "$@"
# vim: ft=zsh