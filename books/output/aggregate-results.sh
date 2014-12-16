#!/bin/bash

for file in `ls . | grep -v agg|grep -v eval|grep -v results`; do
   echo $file  
   cat $file/qrel/* >> $file.qrel
   cat $file/trec/* >> $file.trec
done
