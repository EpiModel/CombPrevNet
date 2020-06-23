#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est

scp burnin/burnin1/*.R burnin/burnin1/*.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/*.rda burnin/burnin1/data/

scp mox:/gscratch/csde/sjenness/combprev/est/burnin1.ATL.3race.rda est/
