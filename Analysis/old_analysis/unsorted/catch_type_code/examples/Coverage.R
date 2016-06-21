# date: 2014-01-14
# goal: Figure out coverage of Observer data for both non-hake and hake

require(plyr)
#####################################
## Non Hake Observer Data (WCGOP?) ##
#####################################
# load data
	# local
	nhObs <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Obs/Data/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")
	
# number of vessels
	length(unique(nhObs$CG)) 
	
# number of fisheries
	unique(nhObs$sector)
	
# number of vessels per fishery
	vesSec <- ddply(nhObs, c("sector","UP_YEAR"), summarize, numVes = length(unique(CG_NUM)))
	
	# plot
	ggplot(vesSec, aes(x = UP_YEAR, y = numVes, fill = sector)) + geom_bar(stat="identity")
	

########################
## Hake Observer Data ##
########################
# load data
	# local
	hObs_haul <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Obs/Data/hakeObs/csvPages/catchHaul/Samhouri_A-SHOP_haul_12.30.13.csv")

# number of vessels
	# my understanding is that the permit goes to the processor boat, so a mothership or catcher processor. There are 16 of these
	length(unique(hObs_haul$PERMIT))
	# each mothership boat has a number of catcher boats returning to it there are 26 of these
	length(unique(hObs_haul$CATCHER_BOAT_ADFG)) 
	
# number of processor boats by vessel type

	vesType <- ddply(hObs_haul, c("VESSEL_TYPE","Year"), summarize, numVes = length(unique(PERMIT)))

	vesTypetot <- ddply(hObs_haul, c("VESSEL_TYPE"), summarize, numVes = length(unique(PERMIT)))

# number of catcher boats per mothership

	catchboats <- ddply(hObs_haul, "PERMIT", summarize, numCatch = length(unique(CATCHER_BOAT_ADFG)))
	