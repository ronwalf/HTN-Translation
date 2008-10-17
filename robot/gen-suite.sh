#!/bin/bash

for (( p=10; p<100; p += 10 )); do
  for (( d=10; d<100; d += 10 )); do
    ./gen-test.sh $d $p 10
  done
done

