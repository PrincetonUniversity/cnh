# tally species by port for jameal

ftl <- read.csv("/Users/efuller/1/CNH/rawData/Catch/FTL_2009-2013_w-o-c_samhouri.csv")
# adjust revenue for 2009 dollars ----
library(quantmod)
getSymbols("CPIAUCSL", src="FRED") # get CPI. The CPI inflation calculator at the BLS uses the latest monthly value for a given year. 

ftl$revenue <- ftl$ppp *ftl$landed_wt

annual.cpi <- CPIAUCSL[.indexmon(CPIAUCSL)==11] # it's two lagged... but want jan for each year

cf <- annual.cpi/as.numeric(annual.cpi['2009']) #using 2009 as the base year
# need to change cf from december to january 
cf <- as.data.frame(cf)
cf$year = strftime(as.Date(row.names(cf)),"%Y")
ftl <- merge(ftl, cf, by = "year", all=FALSE)
ftl$adj_revenue <- ftl$revenue / ftl$CPIAUCSL

ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv")
spid <- read.csv("/Users/efuller/1/CNH/processedData/catch/spid.csv",stringsAsFactors = FALSE)
colnames(spid) <- tolower(colnames(spid))
spid$spid <- tolower(spid$spid)
spid$common_name <- tolower(spid$common_name)
spid$x <- NULL

library(dplyr)
colnames(ftl) <- tolower(colnames(ftl))
colnames(ports) <- tolower(colnames(ports))
ftl$spid <- tolower(ftl$spid)
port_totals <- ftl %>%
  group_by(pcid,spid) %>%
  summarise(revenue = sum(adj_revenue)) %>%
  left_join(ports) %>%
  left_join(spid[,c("spid","common_name")])

forage <- read.csv("/Users/efuller/1/CNH/Analysis/forage_landings/spid_interest.csv",stringsAsFactors = FALSE)
port_totals$forage.fish <- ifelse(port_totals$spid %in% forage$SPID, 1, 0)

write.csv(port_totals, "/Users/efuller/1/CNH/Analysis/forage_landings/port_landings.csv",row.names = FALSE)
