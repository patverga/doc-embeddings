#!/bin/bash

rm centroid sum sdm

for file in `ls sum*`; do IFS='-' read -a id <<< "${file}"; i=${id[1]}; cat sdm-$i >> sdm; cat sum-$i >> sum; cat old/sum-$i >> old-sum; done
