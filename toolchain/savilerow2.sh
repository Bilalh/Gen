#!/bin/bash
set -x
export SR_DIR="$(dirname "$(which savilerow)")";

if [ -f "${SR_DIR}/savilerow2.sh"  ]; then
	exec "${SR_DIR}/savilerow2.sh" "$@"
fi

java -ea -XX:ParallelGCThreads=1 \
	-Xmx"${JAVA_MEM:-16G}"       \
	-jar "$SR_DIR/savilerow.jar" \
	"$@"                         \
	${SR_ARGS}
code=$?
set +x
exit $code