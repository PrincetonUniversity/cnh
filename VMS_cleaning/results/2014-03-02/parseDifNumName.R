# this script shows the number of vessel names associated with multiple CG numbers and vic versa. 

# goal: figuring out what the difference is between vessel name and doc number (dropping duplicates of one will be different than duplicates of other)

rm(list = ls())

require(plyr)
require(sp)
require(corpcor)
require(ggplot2)
require(maps)
require(mapdata)
require(maptools)

# read in data
    VMS <- read.csv("VMS_cleaning/results/2014-03-02/VMS_woDups.csv")

# how doc numbers for each vessel names 
	nameNum <- ddply(VMS,c("Vessel_Name"),summarize,numDoc = length(unique(Ship_Number)))

# quite a number of vessels associated with >1 CG number
	nameNum[which(nameNum$numDoc>1),]

# same for doc numbers and names
	numName <- ddply(VMS,c("Doc_Number"), summarize, numName = length(unique(Vessel_Name)),.progress = "text")

	numName[which(numName$numName > 1),]
	
# are there overlaps between doc numbers that were assigned multiply and the vessel names?
	multNum <- numName$Doc_Number[which(numName$numName>1)]
	vesNam <- vector("list",length(multNum))
	for(i in 1:length(multNum)){
	vesNam[[i]] <- as.character(unique(VMS$Vessel_Name[which(VMS$Doc_Number== multNum[i])]))
	}
	# makes sense that last entry of list is empty because that's for vessels which have no doc number
	
# any overlap between doc numbers assigned to multiple vessels and vessels with multiple doc numbers?
	nameNum[unlist(vesNam %in% nameNum$Vessel_Name)]	# no
	
# this means that duplicates could be found differently depending on whether I use the vessel name or the doc number. 

# will use vessel name to search for duplicates

# duplicates are 100% duplicated rows, but also any two rows that have the same vessel name with the same date and times. so looking through Vessel_Name column, and Date_Time

    dups <- duplicated(VMS[c("Vessel_Name","Date_Time")])
    dVMS <- VMS[dups,]

    VMS <- VMS[-dups,]
	
