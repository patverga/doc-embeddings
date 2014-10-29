#!/bin/bash

for size in {300,800}; do
   for decade in 1850; do
      python gensim-word2vec.py -i decades/$decade -o vectors/${decade}-${size} -s $size
   done
done
