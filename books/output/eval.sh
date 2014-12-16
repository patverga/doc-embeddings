#!/bin/bash

RUN=$1

~/galago-3.6/galago eval --judgments=${RUN}.qrel --runs+${RUN}.trec --details=true --metrics+map --metrics+ndcg20 --metrics+P10

