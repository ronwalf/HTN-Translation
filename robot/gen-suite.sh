#!/bin/bash

for (( d=10; d<100; d += 10 )); do
  for (( p=10; p<100; p += 10 )); do
    echo "On $d $p"
    ./gen-test.sh $d $p $1
  done
done

