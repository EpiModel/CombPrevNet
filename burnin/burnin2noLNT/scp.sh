#!/bin/bash

## MOX ##

# Send
scp est/*.rda mox:/gscratch/csde/sjenness/combprev/est

scp burnin/burnin2noLNT/*.R burnin/burnin2noLNT/*.sh mox:/gscratch/csde/sjenness/combprev

# Receive
scp mox:/gscratch/csde/sjenness/combprev/data/sim.n500.rda burnin/burnin2noLNT/data

scp mox:/gscratch/csde/sjenness/combprev/est/*.rda est/
