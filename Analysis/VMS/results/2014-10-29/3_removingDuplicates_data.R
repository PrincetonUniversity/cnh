# Look for duplicated points
rm(list=ls())

setwd("/Users/efuller/1/CNH/Analysis/VMS/results/2014-10-29/")

library(scales); library(maps); library(data.table)

# read in raw data
  VMS <- readRDS("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_VMS_masked.RDS")
# make it a data.table
  VMSdf <- data.table(VMS@data)
# add lat/lon to it  
VMSdf[, c("Longitude","Latitude") := list(coordinates(VMS)[,1], coordinates(VMS)[,2]), with=FALSE]
# remove original spatial object
  rm(VMS)
# remove duplicates
  setkey(VMSdf, NULL)
  VMSdf <- unique(VMSdf)
# mark duplicates timestamps
  VMSdf[,"alldups" := 
          duplicated(VMSdf, by = c("Ship_Number","Date_Time")) | 
          duplicated(VMSdf,by = c("Ship_Number","Date_Time"), fromLast=TRUE), 
        with=FALSE]
# only keep non-duplicates
  VMSdf <- subset(VMSdf, alldups==FALSE)
# save
  save(VMSdf, file="3_VMSdf.Rdata")

# make WC useful for ggplot2
load("/Users/efuller/1/CNH/Analysis/VMS/writing/code/2_coastline.Rdata")
library(ggplot2)
# make WC a dataframe for ggplot2
WC.points <- fortify(WC,region="id")
ggplot(WC.points) + aes(long,lat, group=group) + geom_polygon() + geom_path(color="white") + coord_equal()
 
