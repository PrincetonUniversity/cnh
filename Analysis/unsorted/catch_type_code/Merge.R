# date: 2014-01-14
# goal: link non-hake observer data to VMS

# load data
	# VMS
	VMS <- readRDS("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/VMS/Code/processed_data/VMSdata.RDS")
	
	# WCGOP Observer data (including shoreside hake)
	nhObs <-  read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Obs/Data/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")
	
	# Hake Observer data (at sea CP and Mothership)
  # [not added yet]

# all VMS vessels
	tracks <- unique(VMS$Doc_Number)

# all non-hake (nh) observed vessels
	obs <- unique(nhObs$CG_NUM)

# observed vessels which have VMS
	which(obs %in% tracks)

# observed vessels which don't have VMS
	which(!obs %in% tracks)
	
	# of which fisheries are these non-VMS vessels a part?
	nonCover <- obs[which(!obs %in% tracks)]
	noVMS <- vector(length=length(nonCover))
	for(i in 1:length(nonCover)){
		sector <- unique(nhObs$sector[which(nhObs$CG == nonCover[i])])
		noVMS[i] <- as.character(sector)
	}

# most are nearshore, but one is limited entry sablefish and the other is OA fixed gear. Let's look at those

	ncSF <- nhObs[which(nhObs$CG == nonCover[11]),]
	oaFG <- subset(nh)bs, CG = nonCover[12])
	
# just realized that these might not be covered because I only have a subset of VMS data.. I think? Come back to this. 

# all hake observed vessels
	hobs <- unique(hObs$CG_NUM)
# observed vessels which don't have VMS
	which(!hobs %in% tracks)
# 12 again... 
