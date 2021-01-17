#!/bin/bash 

# run with ./synch.sh
# make sure you've run chmod 700 synch.sh

# synch plotting project
rsync -av --delete "/Users/samueljclark/Desktop/PNAS Commentary on Indiana Prevalence Method Paper/cdc-cv19-data/" /Users/samueljclark/Documents/GitHub/usaDeaths2020/plot

