#!/bin/bash
 
#SBATCH -o ./out/%x_%a.out
 
source loadR.sh 
Rscript R/03-burnin1-simMin.R