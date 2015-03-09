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

# push ftl_major to della ----
# do metier clustering with infoMap. 
# Bring back dataframe which is ftl_major with a new column (metier)

# calculate annual revenue by vessel ----
  source("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/03_annualRev.R")
  annual_rev <- annualRev(data = ftl_major)