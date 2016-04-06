
# determine correlations between revenues for top10 metiers for specific ports
# TRY NEAH BAY (NEA) AND NEWPORT (NEW)

rm(list=ls())


library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(scales)
library(reshape2)

# set working directory to CNH
setwd("~/Documents/CNH_to_github/cnh/processedData/catch/1_cleaningData")

# read in processed fish ticket data. it's the raw data with a few added columns, revenue in 2009 USD adjusted for inflation. 
#tickets  <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
tickets  <- readRDS("tickets.RDS")
head(tickets)

names(tickets)
# n <- names(tickets)
# write.csv(n, "names of fish ticket data file.csv")

# trip_id is fish ticket ID plus year
# revenue is price per pound * landed weight, CPIAUCSL is the inflation rate, and adj_revenue is (revenue/CPIAUCSL). CPIAUCSL is pulled directly from BLS
# spid is market category for each species, modified is the modified spid that pools a species ID with a nominal species ID. in short, use modified.
# metier.2010 and metier.2012 are the metier designations for each trip using 2010 or 2012 as the base year. we roll with metier.2010

# adjust years so that annual revenues are based on Nov 1 - Oct 31, to account for crab season spanning Dec-Jan

class(tickets$tdate)
tickets$tdate <- as.POSIXlt(tickets$tdate,format = "%d-%b-%y")

# now convert each date to the day of year
tickets$tdate_julian <- as.numeric(format(tickets$tdate, "%j"))


######################################################
######################################################

# estimate correlation in revenue between each pair of 
# metiers, as predicted by year, singlet/duo/3+, maybe 
# region, vessel length. focus on metiers that account 
# for 90% of revenues/yields (n=10)

######################################################
######################################################

# determine which metiers account for top 90% of revenues and yields across 2009-2013, and subset to make a new data frame
tickets$tdate <- as.character(tickets$tdate)

top10 <- tickets %>%
  group_by(metier.2010) %>%
  summarise(dollars = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE)) %>%
  arrange(-dollars) %>% # sort from most to least revenue
  ungroup() %>% 
  mutate(percent.rev = 100*cumsum(as.numeric(dollars))/sum(as.numeric(dollars)), percent.pounds = 100*cumsum(as.numeric(pounds))/sum(as.numeric(pounds))) %>% #calculate cumulative percentage of revenues (or pounds) for each metier across vessels as percentage of revenues (pounds) across all metiers
  filter(percent.rev < 91) # subset to vessels participating in metiers that account for 90% of revenues/yields (n~10)

top10

# add group_by year???
top90byrevenuebyport <- tickets %>%
  group_by(pcid) %>%
  dplyr::select(pcid, drvid, metier.2010, adj_revenue,landed_wt)%>%
  group_by(pcid, metier.2010, drvid) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T), pounds = sum(landed_wt, na.rm=TRUE)) %>%
  group_by(pcid, metier.2010) %>%
  summarize(n_boats = length(unique(drvid)), revenue = sum(revenue), pounds=sum(pounds)) %>%
  filter(n_boats >= 3) %>%
  arrange(-revenue) %>%
  group_by(pcid) %>%
  mutate(cum.percent.rev = 100*cumsum(as.numeric(revenue))/sum(as.numeric(revenue)), 
         cum.percent.pounds = 100*cumsum(as.numeric(pounds))/sum(as.numeric(pounds)),
         has_crab = ifelse(any(metier.2010=="POT_1"), "yes","no")) #%>%
  

group_by(pcid) %>%
  mutate(top90 = ifelse(any(percent.rev < 90), "many", "one")) %>%
  
  transmute()
  
  filter(percent.rev < 91 | max(percent.rev))
  
select(ifelse(any(percent.rev > 90), max(percent.rev), percent.rev < 91))
  

group_by(pcid) %>%
  mutate(n.fisheries = length(unique(metier.2010)), 
            has_crab = ifelse(any(metier.2010=="POT_1"), "yes","no")) 


View(top90byrevenuebyport)
View(subset(top90byrevenuebyport,pcid=="ARE"))

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/output data")
write.csv(top10,"Top 10 metiers by revenue and yields 2009-2013.csv", row.names=FALSE)