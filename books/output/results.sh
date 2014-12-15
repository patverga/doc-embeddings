#!/bin/bash

for measure in `ls *.qrel | rev |cut -c6-|rev`;
do
  echo $measure
  ./eval.sh $measure | tail -3 | head -1 >> results
done
