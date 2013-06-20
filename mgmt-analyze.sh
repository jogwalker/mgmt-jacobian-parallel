#!/bin/bash

#PBS -N mgmt-analysis
#PBS -o job_reports/output-a
#PBS -e job_reports/error-a
#PBS -l walltime=01:00:00

set -e

outdir="~/mgmt-jacobian-parallel/analysis/"
njobs=100

R --no-save --args ${outdir} ${njobs} < ~/mgmt-jacobian-parallel/mgmt-analysis.R
