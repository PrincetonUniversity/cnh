# Goal: Find one vessel which is in Observer data set, parse into trips, and remove any trips for which the vessel leaves the EEZ
require(RColorBrewer)

#-------------------------------------------------------------------
#Step 1: Find a vessel from the observer dataset
  setwd("/Volumes/NOAA_Data/CNH/")
  Obs <- read.csv("Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")

  # which sectors are present?
    unique(Obs$sector)

  # will choose 5 boats from Pink Shrimp, and 5 boats from Limited Entry Trawl and 5 boats from Catch Shares
    # subset by vessel ID, what are the vessel IDs that are associated with Pink Shrimp
    shrimp <- subset(Obs, sector=="Pink Shrimp")
    LE_trawl <- subset(Obs, sector=="Limited Entry Trawl")
    CS <- subset(Obs, sector=="Catch Shares")
  
  # plot locations of trawls for pink shrimp, color by individual on top of bathymetry
      plot_obs <- function(data, color_scheme){
  n_boats = length(unique(data$CG_NUM))
  boaters = unique(data$CG_NUM)
  point_cols = rep(grep(color_scheme,colors(), ignore.case=TRUE, value=TRUE), length.out= n_boats)
  
  for(i in 1:length(unique(data$CG_NUM))){
    if(i==1){
      plot(data$SET_LON[data$CG_NUM==boaters[i]],
           data$SET_LAT[data$CG_NUM==boaters[i]],
           col=point_cols[i],
           ylim=range(data$SET_LAT),
           xlim=range(data$SET_LON),
           cex=.15,pch=19, bty="n",
           xlab="Longitude",ylab="Latitude",
           main=paste("Observed",as.character(unique(data$sector)),sep=" "),asp=1)
    }else{
      points(data$SET_LON[data$CG_NUM==boaters[i]],
             data$SET_LAT[data$CG_NUM==boaters[i]],
             col=point_cols[i],
             cex=.15,pch=19)}
}
map("worldHires",border=FALSE,fill=TRUE, col="grey",add=T)
}

  # LE trawl
    data = LE_trawl; color_scheme="green"
    plot_obs(data, color_scheme)
  # pink shrimp
    data = shrimp; color_scheme="pink"
    plot_obs(data, color_scheme)
  # Catch Shares
    data = CS; color_scheme="blue"
    plot_obs(data, color_scheme)

  # plot the number of tripIDs per vessel, all the same, some variety?
    shrimp_numtrips <- rev(sort(table(shrimp$CG_NUM)))
    plot(shrimp_numtrips,type="h") 
  # take top 5 vessels
    shrimp_top <- names(shrimp_numtrips)[1:5]

  # plot the number of tripIDs per vessel, LE_trawl
    LE_numtrips <- rev(sort(table(LE_trawl$CG_NUM)))
    plot(LE_numtrips,type="h")
    LE_top <- names(LE_numtrips)[1:5]

  # plot the number of tripIDs per vessel, catch shares
    CS_numtrips <- rev(sort(table(CS$CG_NUM)))
    plot(CS_numtrips,type='h')
    CS_top <- names(CS_numtrips)[1:5]

#-------------------------------------------------------------------
# Step 2: take these vessel trajectories from VMS data

shrimp1 <- read.csv(pipe())
