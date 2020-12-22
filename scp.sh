#!/bin/bash

# Send
scp R/*.R mox:/gscratch/csde/sjenness/CombPrevNet/R
scp runsim.sh master-burnin1.sh mox:/gscratch/csde/sjenness/CombPrevNet

# Receive
scp mox:/gscratch/csde/sjenness/CombPrevNet/data/input/*.rds data/input/
scp mox:/gscratch/csde/sjenness/CombPrevNet/data/*.rda data/output/
