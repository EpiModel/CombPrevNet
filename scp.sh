#!/bin/bash

# Send
scp burnin/burnin1/*.R burnin/burnin1/*.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/CombPrevNet/data/input/*.rds data/input/
