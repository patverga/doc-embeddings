#!/bin/bash

for i in {0..250}; do
qsub -cwd -l mem_free=23G -l mem_token=23g -v qid="$i" run_book_query.sh
done
