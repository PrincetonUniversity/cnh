
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
  
  # plot # vessels vs. # metiers participated in for each year
  
  ######################################################
  ######################################################
  setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/code")
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

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/figures")
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

top10

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/output data")
write.csv(top10,"Top 10 metiers by revenue and yields 2009-2013.csv", row.names=FALSE)

### UNADJUSTED YEARS ###

# make data frame identifying strategy used by each vessel (1, 2, or 3+ metiers), with unadjusted years
df <- tickets %>%
  dplyr::select(drvid, year, metier.2010) %>%
  distinct() %>% # give me unique rows
  arrange(drvid, year) %>%
  group_by(drvid, year) %>%
  summarise(n.metiers = length(unique(metier.2010))) %>%
  mutate(number.subset = ifelse(n.metiers == 1, "singlets",
                        ifelse(n.metiers >1 & n.metiers <3,"pairwise",
                        ifelse(n.metiers >= 3, "three plus", NA))))

df

# make data frame we will use to plot correlations in revenue/pounds/trips between metiers, with unadjusted years
df.cor <- tickets %>%
  filter(metier.2010 %in% top10$metier.2010) %>%
  group_by(drvid, year, metier.2010) %>%
  summarise(revenue = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE), trips = length(unique(trip_id))) %>%
  left_join(df)

# df representing revenues from each top10 metier participated in for each vessel by unadjusted year
df.cor.rev <- df.cor %>%
  dplyr::select(-pounds, - trips) %>%
  spread(metier.2010, revenue) # reshape data frame
  
# df representing pounds from each top10 metier participated in for each vessel by unadjusted year
df.cor.lbs <- df.cor %>%
  dplyr::select(-revenue, -trips) %>%
  spread(metier.2010, pounds)  # reshape data frame

# df representing trips from each top10 metier participated in for each vessel by unadjusted year
df.cor.trips <- df.cor %>%
  dplyr::select(-revenue, -pounds) %>%
  spread(metier.2010, trips)  # reshape data frame

### ADJUSTED YEARS ###

# make data frame identifying strategy used by each vessel (1, 2, or 3+ metiers), using adjusted year based on crab season
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

df.adj_year

# make data frame we will use to plot correlations in revenue/pounds/trips between metiers, using adjusted year based on crab season
df.cor.adj_year <- tickets %>%
  mutate(adj_year = ifelse(tdate_julian > 305 & tdate_julian <= 366, 
                           year + 1, year)) %>% # nov 1 is 305 in normal years, 306 in leap years. # assign years between Nov 1, 2009 - Oct, 31, 2010, Nov 1, 2010 - Oct 31, 2011, etc. we will lose first 10 months of 2009 and last 2 months of 2013
  filter(adj_year > 2009 & adj_year < 2014) %>%
  filter(metier.2010 %in% top10$metier.2010) %>%
  group_by(drvid, adj_year, metier.2010) %>%
  summarise(revenue = sum(adj_revenue, na.rm=TRUE), pounds = sum(landed_wt, na.rm=TRUE), trips = length(unique(trip_id))) %>%
  left_join(df.adj_year)

df.cor.adj_year

# df representing revenue from each top10 metier participated in for each vessel by adjusted year
df.cor.rev.adj_year <- df.cor.adj_year %>%
  dplyr::select(-pounds, -trips) %>%
  spread(metier.2010, revenue) # reshape data frame

# df representing pounds from each top10 metier participated in for each vessel by adjusted year
df.cor.lbs.adj_year <- df.cor.adj_year %>%
  dplyr::select(-revenue, -trips) %>%
  spread(metier.2010, pounds)  # reshape data frame

# df representing trips from each top10 metier participated in for each vessel by adjusted year
df.cor.trips.adj_year <- df.cor.adj_year %>%
  dplyr::select(-revenue, -pounds) %>%
  spread(metier.2010, trips)  # reshape data frame

##########################################
# DETERMINE MEAN PROPORTION OF TRIPS FOR EACH OF THE TOP10 METIERS
##########################################

num.trips.by.metier <- colSums(df.cor.trips.adj_year[,-c(1:4)],na.rm=TRUE)
total.trips <- sum(num.trips.by.metier)
mean.prop.trips <- num.trips.by.metier/total.trips

num.trips.out <- data.frame(num.trips.by.metier,mean.prop.trips)
write.csv(num.trips.out,"Total number of trips and mean proportion of trips for top 10 metiers 2009-2013.csv")


##########################################
##########################################
# make pairs plot for metiers by revenue, 
# pounds, trips, and proportion of these 
# quantities
##########################################
##########################################

panel.spearman <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  label <- format(cor(x,y,method="spearman",use="pairwise.complete.obs"), digits=2) #  ifelse(length(which(!is.na(x))) < threshold | length(which(!is.na(y))) <threshold, NA,
  text(horizontal, vertical, label) 
}

### UNADJUSTED YEARS ###
count_dat <- df.cor.rev[,5:14] # subset out to make df of revenues only. columns are top10 metiers, rows are vessel years

# create matrix to store the number of vessel-years with data to estimate correlations between pairs of metiers, with unadjusted years
count_mat <- matrix(data = NA, ncol = ncol(count_dat),nrow=ncol(count_dat))
colnames(count_mat) <- colnames(count_dat)
rownames(count_mat) <- colnames(count_dat)

for(i in 1:nrow(count_mat)){
  row_met = rownames(count_mat)[i]
  sub_ves <- count_dat[which(!is.na(count_dat[,i])),]
  for(j in 1:ncol(count_mat)){
    count_mat[i,j] <-  length(which(!is.na(sub_ves[,j])))
  }
}

count_mat

# pairs plot based on unadjusted years
pairs(df.cor.rev[,5:dim(df.cor.rev)[2]], lower.panel = panel.smooth, upper.panel = panel.spearman, col = alpha("steelblue",0.25), cex = 0.5, pch=19)


### ADJUSTED YEARS ###
count_dat_adj_year <- df.cor.rev.adj_year[,5:14] # subset out to make df of revenues only. columns are top10 metiers, rows are vessel years

# create matrix to store the number of vessel-years with data to estimate correlations between pairs of metiers, with unadjusted years
count_mat_adj_year <- matrix(data = NA, ncol = ncol(count_dat_adj_year),nrow=ncol(count_dat_adj_year))
colnames(count_mat_adj_year) <- colnames(count_dat_adj_year)
rownames(count_mat_adj_year) <- colnames(count_dat_adj_year)

for(i in 1:nrow(count_mat_adj_year)){
  row_met = rownames(count_mat_adj_year)[i]
  sub_ves <- count_dat_adj_year[which(!is.na(count_dat_adj_year[,i])),]
  for(j in 1:ncol(count_mat_adj_year)){
    count_mat_adj_year[i,j] <-  length(which(!is.na(sub_ves[,j])))
  }
}

count_mat_adj_year
       
# make pairs plot based on adjusted years
# iMac
#setwd("/Users/jameal.samhouri/Documents/CNH_shared/Analysis/ses_network")
# macbook
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/Analysis/ses_network")

# REVENUES #

# create a df of rank correlation values for revenues
m1.rev <- cor(df.cor.rev.adj_year[,5:dim(df.cor.rev.adj_year)[2]], method="spearman",use="pairwise.complete.obs")
length(which(is.na(m1.rev)))

# create a 2nd df where we ignore correlations where number of vessels that participate in pair of metiers is <cutoff
cutoff <- 10 # minimum number of vessels for performing correlation test

# Emma's code to record individual vessels rather than vessel-years, produce count_participation that is used to filter out metiers with >=10 vessels

# find number of vessels that ever participated in pairs of metiers across all years
count_participation <- matrix(ncol=ncol(m1.rev), nrow = nrow(m1.rev))
colnames(count_participation) <- colnames(m1.rev)
rownames(count_participation) <- rownames(m1.rev)

for(i in 1:nrow(count_participation)){
  for(j in 1:ncol(count_participation)){
    m1 <- rownames(count_participation)[i]
    m2 <- colnames(count_participation)[j]
    sub_tix <- tickets %>% # this is where we subset to individual vessels rather than vessel-years
      filter(metier.2010 %in% c(m1, m2)) %>%
      dplyr::select(drvid, metier.2010) %>%
      distinct() 
    count_participation[i,j] <- length(which(table(sub_tix$drvid)==2))
  }
}

m1.rev.ignore <- m1.rev
m1.rev.ignore[which(count_participation < cutoff)] <- NA
length(which(is.na(m1.rev.ignore)))

# make corr matrix into a lower diagonal matrix only
m1.rev.ignore[upper.tri(m1.rev.ignore)] <- NA #, diag=TRUE
m1.rev.ignore

write.csv(m1.rev.ignore, "Correlations between metiers based on revenues, adjusted years.csv")

# https://briatte.github.io/ggcorr/
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
# nba = read.csv("http://datasets.flowingdata.com/ppg2008.csv")
# head(nba)

ggcorr(data=NULL, cor_matrix = m1.rev.ignore, geom = "blank", label = TRUE, label_round = 2, size=4) +
  geom_point(size = 12, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.2)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  theme(axis.line=element_blank())+
  ggtitle("Correlation in revenues,\nadjusted years")

ggsave("Correlation matrix based on revenues, adjusted years.pdf")

# POUNDS #

# create a df of rank correlation values for pounds
m1.lbs <- cor(df.cor.lbs.adj_year[,5:dim(df.cor.lbs.adj_year)[2]], method="spearman",use="pairwise.complete.obs")
length(which(is.na(m1.lbs)))

# create a 2nd df where we ignore correlations where number of vessels that participate in pair of metiers is <cutoff
cutoff <- 10 # minimum number of vessels for performing correlation test
m1.lbs.ignore <- m1.lbs
m1.lbs.ignore[which(count_participation  < cutoff)] <- NA
length(which(is.na(m1.lbs.ignore)))

# make corr matrix into a lower diagonal matrix only
m1.lbs.ignore[upper.tri(m1.lbs.ignore)] <- NA #, diag=TRUE
m1.lbs.ignore

write.csv(m1.lbs.ignore, "Correlations between metiers based on pounds, adjusted years.csv")

# https://briatte.github.io/ggcorr/
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
# nba = read.csv("http://datasets.flowingdata.com/ppg2008.csv")
# head(nba)

ggcorr(data=NULL, cor_matrix = m1.lbs.ignore, geom = "blank", label = TRUE, label_round = 2, size=4) +
  geom_point(size = 12, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.2)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  theme(axis.line=element_blank())+
  ggtitle("Correlation in pounds,\nadjusted years")

ggsave("Correlation matrix based on pounds, adjusted years.pdf")

# TRIPS #

# create a df of rank correlation values for revenues
m1.trips <- cor(df.cor.trips.adj_year[,5:dim(df.cor.trips.adj_year)[2]], method="spearman",use="pairwise.complete.obs")
length(which(is.na(m1.trips)))

# create a 2nd df where we ignore correlations where number of vessels that participate in pair of metiers is <cutoff
cutoff <- 10 # minimum number of vessels for performing correlation test
m1.trips.ignore <- m1.trips
m1.trips.ignore[which(count_participation  < cutoff)] <- NA
length(which(is.na(m1.trips.ignore)))

# matrix for FCM analysis
write.csv(m1.trips.ignore, "Correlations between metiers based on trips (symmetric matrix), adjusted years.csv")

# make corr matrix into a lower diagonal matrix only
m1.trips.ignore[upper.tri(m1.trips.ignore)] <- NA #, diag=TRUE
m1.trips.ignore

write.csv(m1.trips.ignore, "Correlations between metiers based on trips, adjusted years.csv")

# https://briatte.github.io/ggcorr/
source("https://raw.githubusercontent.com/briatte/ggcorr/master/ggcorr.R")
# nba = read.csv("http://datasets.flowingdata.com/ppg2008.csv")
# head(nba)

ggcorr(data=NULL, cor_matrix = m1.trips.ignore, geom = "blank", label = TRUE, label_round = 2, size=4) +
  geom_point(size = 12, aes(color = coefficient > 0, alpha = abs(coefficient) > 0.2)) +
  scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
  guides(color = FALSE, alpha = FALSE) +
  theme(axis.line=element_blank())+
  ggtitle("Correlation in trips,\nadjusted years")

ggsave("Correlation matrix based on trips, adjusted years.pdf")





### NEXT STEPS
#1 REDO PAIRS PLOTS AND SAVE
#2 PREDICT CORRELATIONS BETWEEN METIERS BASED ON YEAR, STRATEGY (MAYBE YEAR, VESSEL SIZE)
  #A) USE THESE CORRELATIONS TO DEFINE INTERACTION STRENGTHS IN UNDIRECTED NETWORKS. MAKE DIAGNOAL MATRICES REPRESENTING UNDIRECT NETWORKS BASED ON CORRELATIONS OF REVENUES, POUNDS, TRIPS
#3 RUN FCM TO EQUILIBRIUM AND WITH SIMULATED CRAB CLOSURE FOR COASTWIDE NETWORK - JAMEAL'S HOMEWORK
#4 EMMA'S HOMEWORK: ANALYZE STRUCTURAL PROPERTIES OF COASTWIDE UNDIRECTED PARTICIPATION NETWORK BASED ON INTERACTIONS STRENGTHS FROM CORRELATIONS VS (PROPRTION OF VESSELS THAT DO I AND J RELATIVE TO TOTAL NUMBER THAT DO I OR J), SUGGEST PORTS WITH SUFFICIENT NUMBER OF VESSELS TO REPEAT THIS ANALYSIS ON
#5 USING PORTS SUGGESTED BY EMMA, PARAMETERIZE PARTICIPATION NETWORKS AND ANALYZE STRUCTURAL PROPERTIES OF 




################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################
################################################

################# GRAVEYARD ####################

################################################
################################################
################################################
################################################
################################################

# subset out pairs of metiers with less than a cutoff number of vessels that do both

#cutoff <- 10 # minimum number of vessels for performing correlation test
# for(i in 5:(dim(df.cor.rev)[2]-1)){
#   vessels.that.do.both <- intersect(which(!is.na(df.cor.rev[,i])), which(!is.na(df.cor.rev[,i+1])))
#   num.vessel.temp <- nrow(df.cor.rev[vessels.that.do.both,])
#   if(num.vessel.temp < cutoff) {
#     df.cor.rev[vessels.that.do.both,c(i+4,i+5)] <- NA
#   }            
# }

pairs(df.cor.lbs.adj_year[,5:dim(df.cor.lbs.adj_year)[2]], lower.panel = panel.smooth, upper.panel = panel.spearman, col = alpha("steelblue",0.25), cex = 0.5, pch=19)
  
# old school method for correlation pairs plot
pdf("Pairs plot based on revenues, adjusted years.pdf", width = 10, height = 7)
pairs(df.cor.rev.adj_year[,5:dim(df.cor.rev.adj_year)[2]], lower.panel = panel.smooth, upper.panel = panel.spearman, col = alpha("steelblue",0.25), cex = 0.5, pch=19)
dev.off()

# regular ggplot approach for correlation pairs plot
m1.ignore.melt <- melt(m1.ignore, id.vars=rownames(m1.ignore))
ggplot(m1.ignore.melt, aes(x=Var1,y=Var2,fill=value,colour=value))+
  geom_tile()+
  scale_fill_gradient2(low="red",mid="white",high="blue")+
  scale_colour_gradient2(low="red",mid="white",high="blue")+
  theme_acs()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="top")+
  xlab("")+
  ylab("")


