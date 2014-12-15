#!/bin/bash

java -Xmx23g -cp target/doc-embeddings-0.0-jar-with-dependencies.jar co.pemma.books.BookQueries $qid
