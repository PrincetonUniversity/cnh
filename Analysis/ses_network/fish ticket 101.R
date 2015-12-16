
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyr)
library(scales)

# set working directory to CNH
setwd("/Users/jameal.samhouri/Documents/CNH_shared")

# read in processed fish ticket data. it's the raw data with a few added columns, revenue in 2009 USD adjusted for inflation. 
tickets  <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
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

# plot # vessels vs. # metiers participated in for each year

######################################################
######################################################
setwd("~/Documents/CNH_temp")
source("theme_acs.R")

# start by producing a data frame with vessel - year - number of metiers participated in
df <- tickets %>%
  dplyr::select(drvid, year, metier.2010) %>%
  distinct() %>% # give me unique rows
  arrange(drvid, year) %>%
  group_by(drvid, year) %>%
  summarise(n.metiers = length(unique(metier.2010)))


# make histogram
ggplot(df, aes(x = n.metiers)) +
  geom_histogram(stat = "bin") +
  facet_wrap(~year,ncol=1,scales="free_y") +
  theme_acs()

ggsave("histogram of n.vessels vs. n.metiers participated in for each year.pdf")

# do most vessels participate in <3 metiers each year? YES, but an equal number participate in 2 vs 3+ metiers each year
n.connect <- df %>%
  mutate(number.subset = ifelse(n.metiers == 1, "singlets",
                          ifelse(n.metiers >1 & n.metiers <3,"pairwise",
                          ifelse(n.metiers >= 3, "three plus", NA)))) %>%
  group_by(number.subset, year) %>%
  summarize(n.ves = length(unique(drvid)))

n.connect
# YES, but an equal number participate in 2 vs 3+ metiers each year

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

# make data frame identifying strategy used by each vessel (1, 2, or 3+ metiers)
df <- tickets %>%
  dplyr::select(drvid, year, metier.2010) %>%
  distinct() %>% # give me unique rows
  arrange(drvid, year) %>%
  group_by(drvid, year) %>%
  summarise(n.metiers = length(unique(metier.2010))) %>%
  mutate(number.subset = ifelse(n.metiers == 1, "singlets",
                        ifelse(n.metiers >1 & n.metiers <3,"pairwise",
                        ifelse(n.metiers >= 3, "three plus", NA))))

# make data frame we will use to plot correlations in revenue (pounds) between metiers
df.cor <- tickets %>%
  filter(metier.2010 %in% top10$metier.2010) %>%
  group_by(drvid, year, metier.2010) %>%
  summarise(revenue = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE), trips = length(unique(trip_id))) %>%
  left_join(df)

df.cor.rev <- df.cor %>%
  dplyr::select(-pounds, - trips) %>%
  spread(metier.2010, revenue) # reshape data frame
  

df.cor.lbs <- df.cor %>%
  dplyr::select(-revenue, -trips) %>%
  spread(metier.2010, pounds)  # reshape data frame

df.cor.trips <- df.cor %>%
  dplyr::select(-revenue, -pounds) %>%
  spread(metier.2010, trips)  # reshape data frame

# make data frame we will use to plot correlations in revenue (pounds) between metiers, using adjusted year based on crab season
# make data frame identifying strategy used by each vessel (1, 2, or 3+ metiers)
df.adj_year <- tickets %>%
  dplyr::select(drvid, year, tdate_julian, metier.2010) %>%
  mutate(adj_year = ifelse(tdate_julian > 305 & tdate_julian <= 366, 
                           year + 1, year)) %>% # nov 1 is 305 in normal years, 306 in leap years. # assign years between Nov 1, 2009 - Oct, 31, 2010, Nov 1, 2010 - Oct 31, 2011, etc. we will lose first 10 months of 2009 and last 2 months of 2013
  filter(adj_year > 2009 & adj_year < 2014) %>%
  distinct() %>% # give me unique rows
  arrange(drvid, adj_year) %>%
  group_by(drvid, adj_year) %>%
  summarise(n.metiers = length(unique(metier.2010))) %>%
  mutate(number.subset = ifelse(n.metiers == 1, "singlets",
                                ifelse(n.metiers >1 & n.metiers <3,"pairwise",
                                       ifelse(n.metiers >= 3, "three plus", NA))))

df.cor.adj_year <- tickets %>%
  mutate(adj_year = ifelse(tdate_julian > 305 & tdate_julian <= 366, 
                           year + 1, year)) %>% # nov 1 is 305 in normal years, 306 in leap years. # assign years between Nov 1, 2009 - Oct, 31, 2010, Nov 1, 2010 - Oct 31, 2011, etc. we will lose first 10 months of 2009 and last 2 months of 2013
  filter(adj_year > 2009 & adj_year < 2014) %>%
  filter(metier.2010 %in% top10$metier.2010) %>%
  group_by(drvid, adj_year, metier.2010) %>%
  summarise(revenue = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE), trips = length(unique(trip_id))) %>%
  left_join(df.adj_year)

df.cor.rev.adj_year <- df.cor.adj_year %>%
  dplyr::select(-pounds, -trips) %>%
  spread(metier.2010, revenue) # reshape data frame

df.cor.lbs.adj_year <- df.cor.adj_year %>%
  dplyr::select(-revenue, -trips) %>%
  spread(metier.2010, pounds)  # reshape data frame

df.cor.trips.adj_year <- df.cor.adj_year %>%
  dplyr::select(-revenue, -pounds) %>%
  spread(metier.2010, trips)  # reshape data frame

##########################################
##########################################
# make pairs plot for metiers by revenue, 
# pounds, trips, and proportion of these 
# quantities
##########################################
##########################################
threshold <- 5 # minimum number of vessels for performing correlation test

panel.spearman <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  label <- format(cor(x,y,method="spearman",use="pairwise.complete.obs"), digits=2) #  ifelse(length(which(!is.na(x))) < threshold | length(which(!is.na(y))) <threshold, NA,
  text(horizontal, vertical, label) 
}

# pairs plot based on normal years
pairs(df.cor.rev[,5:dim(df.cor.rev)[2]], lower.panel = panel.smooth, upper.panel = panel.spearman, col = alpha("steelblue",0.25), cex = 0.5, pch=19)

# pairs plot based on adjusted years
pairs(df.cor.rev.adj_year[,5:dim(df.cor.rev.adj_year)[2]], lower.panel = panel.smooth, upper.panel = panel.spearman, col = alpha("steelblue",0.25), cex = 0.5, pch=19)

pairs(df.cor.rev.adj_year[,5:dim(df.cor.rev.adj_year)[2]])

### NEXT STEPS
#2 REDO PAIRS PLOTS AND SAVE
#3 PREDICT CORRELATIONS BETWEEN METIERS BASED ON YEAR, STRATEGY (MAYBE YEAR, VESSEL SIZE)
  #A) USE THESE CORRELATIONS TO DEFINE INTERACTION STRENGTHS IN UNDIRECTED NETWORKS





  
cor(df.cor.rev[,7], df.cor.rev[,9], use="complete.obs")

ggplot(df.cor.rev, aes(x=Var1,y=Var2,fill=value,colour=value))+
  geom_tile()+
  scale_fill_gradient(low="white",high="orange")+
  scale_colour_gradient(low="white",high="orange")+
  theme_acs()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top")+
  xlab("")+
  ylab("")



