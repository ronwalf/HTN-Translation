#!/bin/bash

for (( d=10; d<=200; d += 10 )); do
  echo "On $d"
  ./gen-test.sh $d $1
done

