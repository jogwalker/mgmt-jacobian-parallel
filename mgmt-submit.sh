#!/bin/bash

#PBS -N mgmt-jacobian
#PBS -j oe job_reports/
#PBS -m abe
#PBS -t 1-10
#PBS -l walltime=00:01:00

set -e

if [ -n "${1}" ]; then
  echo "${1}"
  PBS_ARRAYID=${1}
fi

i=${PBS_ARRAYID}

outdir="~/mgmt-jacobian-parallel/results"

R --no-save --args ${outdir} ${i} < ~/mgmt-jacobian-parallel/mgmt-run.R
