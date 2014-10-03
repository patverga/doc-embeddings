#!/bin/bash

rm experiments.result

for f in `ls results`; do ./galago-eval.sh results/$f | tail -n -3 | head -1 | awk -v var="$f" '{print(var, $2)}' >> experiments.result; done
