# Make file
rm(list=ls())
# subset to landing data from "full-income" vessels ----
source("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/01_incomeFilter.R")

# set 10,000 as threshold for median income
  # loads fish ticket data within function
  ftl <- incomeFilter(median_thresh = 10000)

# filter for rarely caught species ----
  source("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/02_filter_rare.R")
  ftl_major <- filter_rare(data = ftl)
  saveRDS(ftl_major, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/filtered_ftl.RDS")

# add metiers to trips ----
# do metier clustering with infoMap: /tigress/efuller/raw_infoMap 
# combine clustering results with filtered ticket data, requires directory 2_defineMetiers has infoMap results
  source("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/03_sewTrips.R")
  tickets <- sewTrips()
  # Bring back dataframe which is ftl_major with a new column (metier)
  # also saves this dataframe in 

# add fishing participation profile to trips ----
# also on della, see fisheries_participation_profiles for more info
  rm(tickets)
  tickets <- readRDS(
    "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/fisheries_participation_profiles/tickets_plus.RDS")
# calculate annual revenue by vessel ----
  source("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/04_annualRev.R")
  yrdf <- annualRev(data = tickets)
