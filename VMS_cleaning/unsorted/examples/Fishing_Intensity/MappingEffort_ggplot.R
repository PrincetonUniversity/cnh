rm(list=ls())
library(ggmap)
library(ggplot2)
library(ggthemes)
library(reshape2)

# if local
#setwd("~/Documents/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Code/processed_data")

# if ursus
setwd("~/NOAA/Data_Analysis/Code/processed_data")

if(!exists("VMSdata")){VMSdata <- readRDS("processedVMS.RDS")}

VMSdata <- as.data.frame(VMSdata)

# filter for speeds < 3.5 km
VMSdata <- subset(VMSdata, 3.5 >= Avg_Speed)
bestcoast <- map_data("usa")

Decs <- unique(VMSdata$Declarations)
Decs <- Decs[-which(is.na(Decs))]

for(i in 1:length(Decs)){
    SubFish <- subset(VMSdata, Declarations == Decs[i] & onland == 0 & is.na(status))
    SubFish$year <- factor(format(SubFish$Date_Time, '%Y'))

   # FishPlot <- ggplot(SubFish, aes(x = Longitude, y = Latitude))

   #  polygons <- 
   #     FishPlot +
   #     geom_point(size = 0.15, color = "#00A0B0") +
   #     geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group)) +
   #     coord_map(xlim=range(SubFish$Longitude) + c(-0.5,0.5), ylim = range(SubFish$Latitude) + c(-0.5, 0.5)) +
   #     theme(legend.position="none") +
   #     theme_few() +
   #     stat_density2d(aes(fill = ..level..),bins = 5,geom="polygon", alpha = 0.75) +
   #     facet_wrap(~ year,ncol=3) +
   #     scale_fill_gradient(low="#EDC951",high="#CC333F")

   # ggsave(filename=paste("../inst/examples/Fishing_Intensity/Plots/",Decs[i],"Polygons.png",sep=""), plot = polygons, scale = 1, width = 9, height = 7, dpi = 300)

   # contours <-
   #     FishPlot + 
   #     geom_point(size = 0.15, alpha = 0.25, color="#00A0B0") + 
   #     geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group), alpha = 0.5) + 
   #     coord_map(xlim=range(SubFish$Longitude) + c(-0.5,0.5), ylim = range(SubFish$Latitude) + c(-0.5, 0.5)) + 
   #     geom_density2d(aes(x = Longitude, y = Latitude, colour = ..level..), bins = I(5), fill = NA, alpha = I(1/2)) +   
   #     scale_colour_gradient2('Fishing\nDensity', 
   #     low = 'darkblue', high = 'red') + 
   #     facet_wrap(~ year,ncol=3) + 
   #     theme_few()

   # ggsave(filename=paste("../inst/examples/Fishing_Intensity/Plots/",Decs[i],"Contours.png",sep=""), plot = contours, scale = 1, width = 9, height = 7, dpi = 300)

    # tiling 

    gridsize = 0.25    # size of grid, in degrees
    SubFish$biglatgrid = floor(SubFish$Latitude/gridsize)*gridsize + gridsize/2   # round to nearest grid center
    SubFish$biglongrid = floor(SubFish$Longitude/gridsize)*gridsize + gridsize/2

    # subset each year
    SubFish09 <- subset(SubFish, year == 2009)
    SubFish10 <- subset(SubFish, year == 2010)
    SubFish11 <- subset(SubFish, year == 2011)
    SubFish12 <- subset(SubFish, year == 2012)
    SubFish13 <- subset(SubFish, year == 2013)


    counts09 <- table(SubFish09$biglongrid,SubFish09$biglatgrid)
    counts09 <- melt(counts09)
    counts09 <- subset(counts09, value > 0)

    counts10 <- table(SubFish10$biglongrid,SubFish10$biglatgrid)
    counts10 <- melt(counts10)
    counts10 <- subset(counts10, value > 0)

    counts11 <- table(SubFish11$biglongrid,SubFish11$biglatgrid)
    counts11 <- melt(counts11)
    counts11 <- subset(counts11, value > 0)

    counts12 <- table(SubFish12$biglongrid,SubFish12$biglatgrid)
    counts12 <- melt(counts12)
    counts12 <- subset(counts12, value > 0)

    counts13 <- table(SubFish13$biglongrid,SubFish13$biglatgrid)
    counts13 <- melt(counts13)
    counts13 <- subset(counts13, value > 0)

    countyear <- c(rep(2009,nrow(counts09)), rep(2010, nrow(counts10)), rep(2011, nrow(counts11)), rep(2012, nrow(counts12)), rep(2013, nrow(counts13)))

    countall <- rbind(counts09, counts10, counts11, counts12, counts13)
    names(countall) <- c("long","lat","hours")
    countall$year <- countyear

    tiles <- 
        ggplot(countall, aes(x = long, y = lat)) + 
        geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group))  +
        geom_tile(aes(fill=log(hours))) + 
        theme_few() + 
        facet_wrap(~ year, ncol = 3) +
        coord_map(xlim=range(SubFish$Longitude) + c(-0.5,0.5), ylim = range(SubFish$Latitude) + c(-0.5, 0.5))

    ggsave(filename=paste("../inst/examples/Fishing_Intensity/Plots/",Decs[i],"Tiles.png",sep=""), plot = tiles, scale = 1, width = 9, height = 7, dpi = 300)
    print(paste("Declaration ",i, " out of ", length(Decs),sep = ""))
}
