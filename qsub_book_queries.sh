#!/bin/bash


# default = short
QUERY_SET="short"
END=248
if [ "$#" == 1 ]; then 
   if [ $1 = "long" ]; then
      QUERY_SET="long"
      END=24
   elif [ $1 = "3" ]; then
      QUERY_SET=3
      END=131
   fi
fi


echo "$QUERY_SET - $END"
for (( i=0; i<=$END; i++ )) do
#   qsub -cwd -l mem_free=23G -l mem_token=23g -v qid="--qid=$i --test=false --query-set=$QUERY_SET" run_book_query.sh
done

