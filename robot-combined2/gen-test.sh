#!/bin/bash


for (( i=${1}; i<=${2}; i+=10 )); do
  ../../genRobot2 ${i} prob${i}
done

