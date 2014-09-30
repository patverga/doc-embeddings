#!/bin/bash

for i in {601..700};
do
  qsub -cwd -l mem_free=47G -v ARGS="$i" qsub-job.sh
done
