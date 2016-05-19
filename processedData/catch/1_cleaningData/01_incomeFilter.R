# Adjust income for 2009 (remove inflation), drop vessels with less than $10,000 median income
incomeFilter <- function(){
  # script does two things: adjusts for 2009 prices
  # calculates median annual income and removes vessels which have less than median_income.
  # can set median income value however

  library(reshape2); library(plyr); library(dplyr); library(scales); library(quantmod); library(lubridate)

# load data----

# create list of data file names
fp <- "rawData/Catch/2006-2015_pacfin_data/"
fn <-dir("rawData/Catch/2006-2015_pacfin_data")
fn <- paste0(fp,fn)

# read in data files
fn <- fn[2:length(fn)]
fn <- as.list(fn)
dl <- lapply(fn, function(x) read.csv(x, stringsAsFactors = FALSE))
head(dl[[1]])

# paste all the data files together
dat <- do.call(rbind,dl)
ftl <- dat[-which(duplicated(dat)),]
colnames(ftl) <- tolower(colnames(ftl))

################################################################
################################################################
# now we start processing the data for all years to adjust to 2009 income
################################################################
################################################################

# because ftid (fish ticket ID) is not unique by year, compile ftid and year together
ftl$trip_id <- paste0(ftl$ftid, ftl$year)

# remove vessel 0, *****, ^ZZZ, and UNKNOWN
if(length(grep(pattern = "[****]",ftl$drvid)>0)){
  ftl <- ftl[-grep(pattern = "[****]",ftl$drvid),]  
}

if(length(which(ftl$drvid %in% c("0","UNKNOWN")))>0){
  ftl <- ftl[-which(ftl$drvid %in% c("0","UNKNOWN")),]  
}

if(length(grep("^ZZZ",ftl$drvid))>0){
  ftl <- ftl[-grep("^ZZZ",ftl$drvid),]
}

# only keep commercial and commercial directed type fisheries (drop, scientific, tribal, etc)
ftl <- ftl[which(ftl$removal_type %in% c("C","D")),]

# Find annual income, adjust to 2009 ----

# Adjust income to 2009 using CPI 
# (see here http://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package)
getSymbols("CPIAUCSL", src="FRED") # get CPI. 
# The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 


ftl$revenue <- ftl$ppp *ftl$pounds

annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] # it's two lagged... but want jan for each year

cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
# need to change cf from december to january 
cf <- as.data.frame(cf)
cf$year = strftime(as.Date(row.names(cf)),"%Y")
ftl <- merge(ftl, cf, by = "year", all=FALSE)
ftl$adj_revenue <- ftl$revenue / ftl$CPIAUCSL

return(ftl)

#----
}