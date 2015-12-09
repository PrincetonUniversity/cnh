# Make file
rm(list=ls())
# subset to landing data from "full-income" vessels ----
source("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/01_incomeFilter.R")

# set 10,000 as threshold for median income
  # loads fish ticket data within function
  ftl <- incomeFilter()

# filter for rarely caught species ----
  source("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/02_filter_rare.R")
  ftl_major <- filter_rare(data = ftl)
  saveRDS(ftl_major, "/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/filtered_ftl.RDS")

# add metiers to trips ----
# do metier clustering with infoMap: /tigress/efuller/raw_infoMap 
# combine clustering results with filtered ticket data, requires directory 2_defineMetiers has infoMap results
  source("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/03_sewTrips.R")
  sewTrips(base_year=2010) # base year is which clustering results to use. 
  sewTrips(base_year=2012)

  # load these data, combine so have both metier classifications
  d10 <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets_2010.RDS")
  d12 <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets_2012.RDS")
  
  library(dplyr)
  d10 <- d10 %>%
    select(trip_id, metier) %>%
    rename(metier.2010 = metier) %>%
    distinct() 
  
    tickets <- merge(d12, d10) %>%
    rename(metier.2012 = metier)
  
  saveRDS(tickets, "/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")

# compare classification schemes ----
 
# not doing below for now, may delete. 
# # add fishing participation profile to trips ----
# # also on della, see fisheries_participation_profiles for more info
#   rm(tickets)
#   tickets <- readRDS(
#     "/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")
# # calculate annual revenue by vessel ----
#   source("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/04_annualRev.R")
#   yrdf <- annualRev(data = tickets)
