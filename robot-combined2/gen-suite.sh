#!/bin/bash

for (( d=1; d<=$1; d += 1 )); do
  TGT=run.${d}
  rm -rf ${TGT}/
  mkdir ${TGT}/
  ln ../genRobot2 ${TGT}/
  ln gen-test.sh ${TGT}/
  ln ff ${TGT}/
  ../domainTranslate 1 robot.hpddl > ${TGT}/robot.hpddl
done

