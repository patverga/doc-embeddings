#!/bin/bash

/home/pat/galago/galago eval --judgments=/home/pat/doc-embeddings/data/robust04.qrels --runs=/home/pat/doc-embeddings/out/$1  --details=true --metrics+map
