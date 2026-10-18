#!/bin/bash

sbatch -p csde -A csde --array=1  --nodes=1 --ntasks-per-node=35 --time=00:30:00 --mem=120G --job-name=s1000 --export=ALL,SIMNO=1000,NJOBS=1,NSIMS=35 runsim.sh
