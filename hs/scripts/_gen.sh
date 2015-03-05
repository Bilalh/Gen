# Completion for gen
_gen()
{
    # local CMDARGS_DEBUG=1 # uncomment to debug this script


    local last=${COMP_WORDS[COMP_CWORD-1]}
    if [ "${last}x" = "genx" ]; then
        COMPREPLY=( $(compgen -W "essence reduce link meta json script-toolchain" -- $cur) )
        return 0
    fi

    local cur=${COMP_WORDS[COMP_CWORD]}

    if [ "${cur:0:1}x" != '-x' ]; then
        COMPREPLY=( $( compgen -o filename -f  -- $cur ) )
        return 0
    fi

    COMPREPLY=()
    function add { COMPREPLY[((${#COMPREPLY[@]} + 1))]=$1 ; }
    IFS=$'\n\r'

    export CMDARGS_COMPLETE=$((${COMP_CWORD} - 1))
    result=`gen ${COMP_WORDS[@]:1}`

    if [ -n $CMDARGS_DEBUG ]; then
        echo Call \(${COMP_WORDS[@]:1}, $CMDARGS_COMPLETE\) > cmdargs.tmp
        echo $result >> cmdargs.tmp
    fi
    unset CMDARGS_COMPLETE
    unset CMDARGS_COMPLETE_POS

    for x in $result ; do
        case $x in
            VALUE\ *)
                add ${x:6}
                ;;
            FILE\ *)
                COMPREPLY=( $( compgen -o filename -f  -- $cur ) )
                ;;
            *)
                COMPREPLY=( $( compgen -o plusdirs  -f  -- $cur ) )
                ;;
        esac
    done
    unset IFS

    if [ -n $CMDARGS_DEBUG ]; then
        echo echo COMPREPLY: ${#COMPREPLY[@]} = ${COMPREPLY[@]} >> cmdargs.tmp
    fi
}
complete -o bashdefault -F _gen gen
