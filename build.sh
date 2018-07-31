#!/bin/bash

set -ex

export USER=root

make -j

if [ -e model/jags/mcmc-results-quick-run.rds ]; then mv model/jags/mcmc-results-quick-run.rds /output/; fi

if [ -e model/jags/mcmc-results-full-run.rds ]; then mv model/jags/mcmc-results-full-run.rds  /output/; fi




