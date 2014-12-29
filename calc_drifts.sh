#!/bin/bash

echo "Running long queries"
for i in {0..24};do
  echo $i
  java -Xmx15g -cp target/doc-embeddings-0.0-jar-with-dependencies.jar co.pemma.books.TimeDriftTest --qid=$i --test=false --long=true
done

echo "Running short queries"
for i in {0..248};do
  echo $i
  java -Xmx15g -cp target/doc-embeddings-0.0-jar-with-dependencies.jar co.pemma.books.TimeDriftTest --qid=$i --test=false --long=false
done


