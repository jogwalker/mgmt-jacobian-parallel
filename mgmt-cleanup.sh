#!/bin/bash

# submit as ./mgmt-cleanup.sh (make executable chmod u+x mgmt-cleanup.sh)

set -e 

cd ~/mgmt-jacobian-parallel/results
tar cvf ~/mgmt-jacobian-parallel/stable.tar *stable*
tar cvf ~/mgmt-jacobian-parallel/inverses.tar *inverse*
rm *

cd ~/mgmt-jacobian-parallel/job_reports
rm *

cd ~/mgmt-jacobian-parallel/
