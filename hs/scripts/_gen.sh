#!/bin/bash
# Completion for gen
_gen()
{
    local last=${COMP_WORDS[COMP_CWORD-1]}
    # MODIFY THIS ONLY for a new sub-command
    if [ "${last}x" = "genx" ]; then
        COMPREPLY=( $(compgen -W "essence reduce generalise instance-undirected instance-nsample search link json weights script-toolchain script-updateChoices script-removeDups db instance-summary instance-allsols instance-noRacing script-smac-process" -- $cur) )
        return 0
    fi

    local cur=${COMP_WORDS[COMP_CWORD]}

    if [ "${cur:0:1}x" != '-x' ]; then
        COMPREPLY=( $( compgen -f  -- $cur ) )
        return 0
    fi

    COMPREPLY=()
    function add { COMPREPLY[((${#COMPREPLY[@]} + 1))]=$1 ; }
    IFS=$'\n\r'

    export CMDARGS_COMPLETE=$((${COMP_CWORD} - 1))
    result=`gen ${COMP_WORDS[@]:1}`

    unset CMDARGS_COMPLETE
    unset CMDARGS_COMPLETE_POS

    for x in $result ; do
        case $x in
            VALUE\ *)
                add ${x:6}
                ;;
            FILE\ *)
                COMPREPLY=( $( compgen -f  -- $cur ) )
                ;;
            *)
                COMPREPLY=( $( compgen -o plusdirs -f -- $cur ) )
                ;;
        esac
    done
    unset IFS

}

complete -o bashdefault -F _gen gen
