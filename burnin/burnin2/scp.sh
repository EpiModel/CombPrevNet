#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est
scp burnin/burnin2/*.R burnin/burnin2/*.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/sim.n300.rda burnin/burnin2/data

scp mox:/gscratch/csde/sjenness/combprev/est/burnin2.ATL.Prep15.rda est/

