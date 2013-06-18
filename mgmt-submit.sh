#!/bin/bash

#PBS -N test
#PBS -j oe
#PBS -J 1-10

set -e

if [ -n "${1}" ]; then
  echo "${1}"
  PBS_ARRAY_INDEX=${1}
fi

i=${PBS_ARRAY_INDEX}

R --no-save --args ${i} < ~/mgmt-jacobian-parallel/mgmt-run.R
