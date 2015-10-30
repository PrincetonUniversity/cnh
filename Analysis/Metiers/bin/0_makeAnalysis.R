# make file for analysis
rm(list=ls())
# create vessel level data set ----
source("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/01_vessel_diversity_landings.R")

# create port level data set ----
# (depends on the port level landings created)
source("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/02_port_diversity_landings.R")

# run models and make figures ----
source("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/03_fig_drafts.R")