#!/bin/bash

#PBS -N test
#PBS -j oe
#PBS -t 1-10

set -e

if [ -n "${1}" ]; then
  echo "${1}"
  PBS_ARRAYID=${1}
fi

i=${PBS_ARRAYID}

R --no-save --args ${i} < ~/mgmt-jacobian-parallel/mgmt-run.R
