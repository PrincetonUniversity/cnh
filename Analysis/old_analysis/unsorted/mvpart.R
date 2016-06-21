# load ftl
library(reshape2); library(dplyr); library(ade4); library(vegan)
ftl <- read.csv("/Volumes/untitled/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv",stringsAsFactors = F)

catch <- select(ftl, veid, ftid,grgroup, grid, pcid,spid, landed_wt)
catch_melt <- melt(catch, id.vars=c("veid","ftid","grgroup","grid","pcid","spid"), measure.vars = "landed_wt")
by_trip <- dcast(catch_melt, ftid ~ spid + variable,fun.aggregate=sum)

class(by_trip)

# transform to hellinger
trip.norm <- decostand(as.matrix(by_trip[,2:ncol(by_trip)]), "hellinger")

predictors <- catch[,c(1,2,3,4,5,6)]
predictors <- predictors[!duplicated(predictors$ftid),]
predictors <- predictors[,-2]

library(mvpart)

brt.trip <- mvpart(trip.norm ~., predictors, xv = "1se", xval = nrow(trip.norm), xvmult=100, which = 4)
