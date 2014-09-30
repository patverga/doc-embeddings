#!/bin/bash

for i in {301..450};
do
  qsub -cwd -l mem_free=20G -v ARGS="$i" qsub-job.sh
done

