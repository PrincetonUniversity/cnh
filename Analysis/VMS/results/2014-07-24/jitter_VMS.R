# hard code some shrimp vessels. find vessels that do little but shrimp by using FTL tickets to find veids and gear. Any boat that as a grgroup of TWS (shrimp trawl). This can include prawn shrimp, but like, whatever man. (for the moment)

# find shrimp boats

  # load data
  FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=FALSE)

  # find vessel IDs for those that land shrimp
  sv <- unique(subset(FTL, grgroup == "TWS", select = veid)$veid)
  
  # find vessels that do shrimp
  shrimpers <- subset(FTL, veid %in% sv)

  # what kinds of gears besides shrimp - aggregate
  barplot(table(shrimpers$grgroup),las=2, bor=F, col="steelblue")
  
  # what kind of gears besides shrimp - by vessel
    # cannot figure out how to do this tonight, doing a for loop
  gear_combo <- data.frame(veid = sv, gc = rep("hi",length(sv)),stringsAsFactors=F)
  for (i in 1:length(sv)){
    vessel <- subset(shrimpers, veid == sv[i])
    gears <- sort(unique(vessel$grgroup))
    pain <- gsub(" ","",x=toString(gears))
    painful <- gsub(",","",pain)
    gear_combo[i,2] <- as.character(painful)
  }
  
  barplot(table(gear_combo$gc),col="steelblue",bor=F,las=2, ylab="number of vessels with combination of gear")
  
  # number of vessels have just tws. Do any of them have VMS?
  js <- subset(gear_combo, gc == "TWS")
  
  # load VMS
  VMS <- read.csv("/Volumes/NOAA_Data/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv", stringsAsFactors=F)

  just_s <- subset(VMS, Doc_Number %in% js$veid)
  plot(just_s$Longitude,just_s$Latitude,asp=1,pch=19, cex=.15, col = as.numeric(as.factor(just_s$Vessel_Name)))
  map("state",add=T)
  # results in 8 vessels, 
  
  jittered <- just_s
  # change lat/lon into complex number so can be annonymized
  jittered$complex <- jittered$Longitude + 1i * jittered$Latitude
  
  # for each vessel, subtract mean, so that it's centered and can't see relation to one another. This maintains vessels' relationship to itself, but erases relationships between vessels. 
  
  changed <- ddply(jittered, .(Vessel_Name),mutate, newcomplex = complex - mean(complex))
  changed$vessel_id <- as.numeric(as.factor(changed$Vessel_Name))
  
  #lookup code
  changed$ref <- seq(1,nrow(changed))
  
  forAngela <- select(changed, vessel_id, Latitude, Longitude, Date_Time, Avg_Speed, Avg_Direction, onland, newcomplex)
  forAngela$Longitude <- Re(forAngela$newcomplex)
  forAngela$Latitude <- Im(forAngela$newcomplex)
  forAngela$newcomplex <- NULL

  
  foo <- subset(forAngela, vessel_id==8)
plot(foo$Longitude,foo$Latitude, pch=19, cex=.15)
  
  write.csv(forAngela, file="/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-24/jittered_VMS.csv")
  write.csv(changed, file="/Volumes/NOAA_Data/CNH/Analysis/VMS/2014-07-24/ref_jittered.csv")
