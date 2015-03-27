# Adjust income for 2009 (remove inflation), drop vessels with less than $10,000 median income
incomeFilter <- function(median_thresh){
  # script does two things: adjusts for 2009 prices
  # calculates median annual income and removes vessels which have less than median_income.
  # can set median income value however

  library(reshape2); library(plyr); library(dplyr); library(scales)

# load data----

ftl <- read.csv(
  "/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", 
  stringsAsFactors = F)
# because ftid is not unique by year, compile ftid and year together
ftl$trip_id <- paste0(ftl$ftid, ftl$year)

# remove vessel 0, *****, and UNKNOWN
ftl <- ftl[-grep(pattern = "[****]",ftl$veid),]
ftl <- ftl[-which(ftl$veid %in% c("0","UNKNOWN")),]

# Find annual income, adjust to 2009 ----

# Adjust income to 2009 using CPI (see here http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
library(quantmod)
getSymbols("CPIAUCSL", src="FRED") # get CPI. The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 
library(lubridate)

ftl$revenue <- ftl$ppp *ftl$landed_wt

annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] # it's two lagged... but want jan for each year

cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
# need to change cf from december to january 
cf <- as.data.frame(cf)
cf$year = strftime(as.Date(row.names(cf)),"%Y")
ftl <- merge(ftl, cf, by = "year", all=FALSE)
ftl$adj_revenue <- ftl$revenue / ftl$CPIAUCSL

# drop vessels with less than 10,000 median revenue ----
library(dplyr)
grouped_ves <- group_by(ftl, drvid, year)
revenues <- summarise(grouped_ves, revenue = sum(adj_revenue))
annual_rev <- summarise(revenues, median_annual = median(revenue))
vessel_drops <- annual_rev$drvid[which(annual_rev$median_annual < median_thresh)]

# loose almost half of vessels
ftl_major <- ftl[-which(ftl$drvid %in% vessel_drops),]
return(ftl_major)

#----
}