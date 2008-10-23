#!/bin/bash

DIRS="run.*"

function input_files {
  cd ${2}
  FILES="prob*.${1}"
  INPUT=""
  for f in ${FILES} ; do
    INPUT=",${f}${INPUT}"
  done
  INPUT="ff,robot.${1}${INPUT}"
  cd ..
}

for d in ${DIRS} ; do
  input_files ${1} $d
  break;
done



TGT=condor.ff.${1}.cmd
cat template.cmd \
  | sed -e "s/--EXEC--/.\/run-ff.sh/g" \
  | sed -e "s/--FILES--/${INPUT}/g" \
  | sed -e "s/--ARGS--/${1}/g" > ${TGT}


for d in ${DIRS} ; do
  echo "InitialDir = ${d}" >> ${TGT}
  echo "Queue" >> ${TGT}
  echo "" >> ${TGT}
done

