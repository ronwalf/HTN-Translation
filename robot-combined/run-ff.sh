#!/bin/sh

ulimit -t 7200

TIME="/usr/bin/time -p"

FILES="prob*.${1}"

for f in ${FILES}; do
  SOLN=${f}.soln
  $TIME ./ff -o robot.${1} -f ${f} >& ${SOLN}
  grep -q '^ff: found legal plan as follows' ${SOLN}
  if [ $? -ne 0 ]; then
    BADFILE=BADRUN.${1}
    echo "Failure on ${f}" > ${BADFILE}
    cat ${BADFILE}
    exit
  fi  
  tail -3 ${SOLN} >> times.${1}
  tail -3 times.${1}
done
touch GOODRUN.${1}
