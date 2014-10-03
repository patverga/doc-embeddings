#!/bin/bash

declare -a dir=("centroid" "embed-expansion" "rm-expansion" "sdm" "sentence" "sum" "wiki-expansion" "rm-embed-expansion")

for d in ${dir[@]}; do
echo $d
rm results/${d}.result
cat ${d}/* >> results/${d}.result
done

#for file in `ls sdm*`; do IFS='-' read -a id <<< "${file}"; i=${id[1]}; cat sentence-$i >> sentence; cat sum-$i >> sum; cat sdm-$i >> sdm; done
