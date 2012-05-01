#!/usr/bin/env bash
STEPS=50
STEPSIZE=1000
PREFIX=default_

_COUNT=0

rm ${PREFIX}results 2> /dev/null

echo -e $_COUNT '\t' $(./stats.erl short) >> ${PREFIX}results
while [ $_COUNT -lt $STEPS ]; do
  (( _COUNT = _COUNT + 1 ))
  ./tick.erl $STEPSIZE && \
  echo -e $_COUNT '\t' $(./stats.erl short) >> ${PREFIX}results
done
