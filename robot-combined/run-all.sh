#!/bin/bash

DIRS="run.*_*"

for d in ${DIRS}; do
  cd ${d}
  echo "Running $d ff pddl"
  ./run-ff.sh pddl
  echo "Running $d ff hpddl"
  ./run-ff.sh hpddl
  cd ..
done

