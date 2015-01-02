# looking to see how fishermen distribute effort over time. Two main questions: how many patches (here probably grid cells)? And how much effort do they spend in each?

# Look at distribution of time spend in each grid (will give an idea for total number of grid cells and whether time spent in each of them is roughly uniform

# keep in mind that this is aggregated, so if things look uniform seasonality could still play a role, and individuals could have strong territories. If everyone is concentrated in few grids, it's likely that's the true pattern.

rm(list=ls())
library(sp)
library(ggthemes)
library(reshape2)

setwd("~/NOAA/Data_Analysis/Code/processed_data")

if(!exists("VMSdata")){VMSdata <- readRDS("processedVMS.RDS")}

VMSdata <- as.data.frame(VMSdata)

# will subset just crab and pink shrimp. think it's more likely that crab will be territorial, shrimp will use shared trawling grounds

crabs <- subset(VMSdata, Declarations == 261  & onland == 0 & is.na(status))
shrimps <- subset(VMSdata, Declarations == 241 & onland == 0 & is.na(status))

crab.lookup <- melt(table(crabs$longrid, crabs$latgrid))
names(crab.lookup) <- c("Long","Lat","Crab.hours")
shrimp.lookup <- melt(table(shrimps$longrid, shrimps$latgrid))
names(shrimp.lookup) <- c("Long","Lat","Shrimp.hours")

# Use merge to link shrimp to crab lat/lon pairs?

together <- merge(crab.lookup, shrimp.lookup, by = c('Long','Lat'), all.x = TRUE, all.y = TRUE)
together$cellID <- seq(1,nrow(together))

png(file="PatchUse_CrabShrimp.png"), width = 800, height = 500)
par(mfrow = c(1,2))
barplot(together$Crab.hours,col="red", main = "Crab"); barplot(together$Shrimp.hours, col="pink", main = "Shrimp")
dev.off()
# but would really like to look at individual fishermen

# should consider subsetting based on speed as well. (eventually fishing time) because much of this might depend if there is one or many ports boats return to.
