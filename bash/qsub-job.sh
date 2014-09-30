#!/bin/bash
#
#

echo $ARGS

cd /home/pat/work3/doc-embeddings

mvn compile && mvn exec:java -Dexec.mainClass=co.pemma.RobustThings -Dexec.args="${ARGS}"
