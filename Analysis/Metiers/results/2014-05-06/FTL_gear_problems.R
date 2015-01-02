## Author: emma
## Date: 2014-05-06
## Goal: showing that FTL entries for the same trip are recorded with different gear codes. 

load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/log_tripTable_2014-05-01.Rdata")
early_table <- log_tripTable

load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-05-06/log_tripTable_2014-05-06.Rdata")
late_table <- log_tripTable

# i think there are duplicates that weren't removed based on the addition of gear groups

late_ref <- late_table[[2]]

# which are duplicated excluding gear groups

dup_late <- late_ref[which(duplicated(late_ref[,1:3])),]
nrow(dup_late) == nrow(late_table[[2]])-nrow(early_table[[2]])
# this explains it. This suggests that there are multiple gear codes associated with the same trip ID. 

dup_all <- late_ref[which(duplicated(late_ref[,1:3]) | duplicated(late_ref[,1:3], fromLast=TRUE)),]

## this means that different entries of the same vessel on same date are recorded with different gear. 
head(dup_all,20)

# how many different vessels does this happen with?
length(unique(dup_all$veid))

# does this happen in the raw FTL data?
FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE)
require(data.table)
require(dplyr)
setnames(FTL,names(FTL),tolower(names(FTL)))

dates <- paste(FTL$year,FTL$month, FTL$day,sep="-")
FTL$tripID <- paste(dates,FTL$veid,sep="_")
setkey(FTL, tripID)

raw_FTL <- select(FTL, tripID, grgroup, grid, year, veid )
raw_FTL <- raw_FTL[-which(duplicated(raw_FTL)),]

raw_dups <- raw_FTL[which(duplicated(raw_FTL[,c(1,4,5)]) | duplicated(raw_FTL[,c(1,4,5)],fromLast = TRUE)),]

# remove unknowns
raw_dups <- subset(raw_dups, veid!="UNKNOWN")
raw_dups <- raw_dups[order(raw_dups$tripID),]

# yep they exist. And this makes sense given what Alia said (see notebook.md for details)