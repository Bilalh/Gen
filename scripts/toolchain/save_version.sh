#!/bin/bash
set -o nounset

function echoing(){
    echo "$@"
    "$@"
}

_fp="$1$(date +%F_%H-%M_%s)"
fp="`python -c 'import os,sys; print(os.path.realpath(sys.argv[1]))'  ${_fp}`"
echo "" > "${fp}.versions"

if [ ! -f "${fp}.versions" ] ; then
    echo "Can't write to ${fp}.versions"
    exit 1
fi

if ( git rev-parse --is-inside-work-tree ); then
    echo "repo(git) version" >> "${fp}.versions"
    git log -1 --format="%H" >> "${fp}.versions"
    git describe --always    >> "${fp}.versions"
    echoing git branch  >> "${fp}.versions"
    echo "" >> "${fp}.versions"
fi

echo "\$testSample is ${testSample:-unset}"

repos_dir="$(dirname `which savilerow`)/../"

echo "System info" >> "${fp}.versions"
echoing parallel --number-of-cpus  >> "${fp}.versions"
echoing parallel --number-of-cores >> "${fp}.versions"
echo "" >> "${fp}.versions"
echoing hostname >> "${fp}.versions"
echoing whoami >> "${fp}.versions"

echo "" >> "${fp}.versions"

if (sw_vers); then
    echoing sw_vers >>  "${fp}.versions"
    echo "" >> "${fp}.versions"
else
    echoing cat /proc/meminfo  | grep MemTotal >> "${fp}.versions"
    echo "" >> "${fp}.versions"
fi

echo "conjure" >> "${fp}.versions"
conjure 2>&1 | grep Version  >> "${fp}.versions"
echo "" >> "${fp}.versions"
minion 2>&1	 | egrep 'HG version|Minion Version' >> "${fp}.versions"
echo "" >> "${fp}.versions"
savilerow	 | grep Version >> "${fp}.versions"
echo "" >> "${fp}.versions"
${testSample:-testSample} --version >> "${fp}.versions"


echo ""  >> "$fp.versions"
echo "##Repos##"  >> "$fp.versions"
echo ""  >> "$fp.versions"

declare -a repos
repos=(conjure conjure-private conjure-testing essenceAST instancegen instancegen-models minion savilerow)
for repo in "${repos[@]}"; do

    if [ ! -d "$repos_dir/$repo" ]; then
        echo "$repos_dir/$repo"
        continue
    fi
    pushd "$repos_dir/$repo"

    if [ -d ".git" ] ; then
        echo "$repo(git) version" >> "${fp}.versions"
        git log -1 --format="%H" >> "${fp}.versions"
        git describe --always    >> "${fp}.versions"
        echoing git branch  >> "${fp}.versions"
        echo "" >> "${fp}.versions"
    elif [ -d ".hg" ]; then
        echo "$repo(hg) version" >> "${fp}.versions"
        hg log -r . --template "{latesttag}-{latesttagdistance}-{node|short}" >> "${fp}.versions"
        echo "" >> "${fp}.versions"
        echoing hg branch  >> "${fp}.versions"
        echo "" >> "${fp}.versions"
    fi

    echo "" >> "${fp}.versions"
    popd
done



echo "##OTHER##"  >> "$fp.versions"
echo "" >> "${fp}.versions"

declare -a commands
commands=(python python3 pip sqlite3 git hg perl parallel ruby ghc cabal gcc clang pigz pip3 )
for prog in "${commands[@]}"; do
    echo "$prog version:" >> "${fp}.versions"
    $prog --version  2>&1 | cat | head -n3 >> "${fp}.versions"
    echo "" >> "${fp}.versions"
done

# Java just had to be different
echo "java version:" >> "${fp}.versions"
java -version  2>&1 | cat | head -n3 >> "${fp}.versions"
echo "" >> "${fp}.versions"


echoing pip freeze  2>&1  >> "${fp}.versions"
echo "" >> "${fp}.versions"


echoing pip3 freeze  2>&1   >> "${fp}.versions"
echo "" >> "${fp}.versions"

echoing cabal list --installed --simple-output  2>&1  >> "${fp}.versions"
echo "" >> "${fp}.versions"
