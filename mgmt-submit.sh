#!/bin/bash

#PBS -N mgmt-jacobian
#PBS -o job_reports/output
#PBS -e job_reports/error
#PBS -t 1-1000
#PBS -l walltime=01:00:00

# PBS -m abe

set -e

if [ -n "${1}" ]; then
  echo "${1}"
  PBS_ARRAYID=${1}
fi

i=${PBS_ARRAYID}

outdir="~/mgmt-jacobian-parallel/results/"

R --no-save --args ${outdir} ${i} < ~/mgmt-jacobian-parallel/mgmt-run.R
