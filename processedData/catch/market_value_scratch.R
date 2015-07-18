# looking at diversity against market value
yrdf <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/yrdf.RDS")
ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")

# total annual market value = annual adj revenue, sum
library(plyr)
metier_value_annual <- ddply(ftl, .(metier, year), summarize, market_value = sum(adj_revenue))

# for each vessel, for each year, find all metiers it's in 
vessel_mets <- unique(ftl[,c("metier","year","drvid")])

vessel_market.value <- merge(vessel_mets, metier_value_annual, by = c("metier","year"), all.x=TRUE)

# remove any with market value of 0
# TWL_17 is consistently 0, is bycatch of pacific halibut in hake I think. 
# TWL_8 is consistently < 1, is chinook bycatch of hake, I think. 
# HKL_21 is unsp octopus longline, also bycatch
# TWL_12 is midwater canary, bycatch hake?

# Also Mako with MSC gear, but that's only one year. others they get more value. so leave it
vessel_market.value <- vessel_market.value[-which(vessel_market.value$market_value==0),]
vessel_market.value <- vessel_market.value[-which(vessel_market.value$metier=="TWL_8"),]

vessel_div <- merge(vessel_market.value, yrdf[[1]][,c("drvid","year","simpsons")], by = c("drvid","year"))

with(vessel_div, plot(market_value, simpsons, cex =.05))
