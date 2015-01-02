# looking at logbook data
require(plyr); require(dplyr)

lg <- read.csv("Data/Catch/LBK_2013_woc_samhouri.csv", stringsAsFactors = F, skip = 2)
colnames(lg)[1] <- "trip_id"
lg <- lg[-1,]
ftl <- read.csv("Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors = F)


# want to know if all logbook records are in ftl 

log_ftid <- unique(select(lg, ftid)$ftid)
log_ftid <- log_ftid[-which(log_ftid=="")] # get rid of the empty one

length(log_ftid) - length(which(log_ftid %in% unique(ftl$ftid))) 
# missing 13 trips, which ones?

missing <- log_ftid[!(log_ftid %in% unique(ftl$ftid))]

missing_lg <- subset(lg, ftid %in% missing)

# how many vessels in these 13 missing trips?

length(unique(missing_lg$veid)) # 8

# nope, year/month range is fine. 
range(ftl$year)
range(ftl$month)

# are these vessels ever in ftl?

miss_vs <- unique(missing_lg$veid)

all(miss_vs %in% ftl$veid) # all are in there


