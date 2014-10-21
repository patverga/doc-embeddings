#!/bin/bash

for size in {300,800}; do
   for decade in {1850,1910}; do
      python gensim-word2vec.py -i decades/$decade -o vectors/$decade -s $size
   done
done
