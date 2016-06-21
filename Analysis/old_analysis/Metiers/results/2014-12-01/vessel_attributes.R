# adding mean latitude of landings, horse power, and vessel length into yrdf dataset.

#----
# adding len/hp/etc.
#----
rm(list=ls())
yrdf <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf.RDS")
library(data.table); library(plyr); library(scales)
sv <- fread("/Users/efuller/1/CNH/Data/Catch/sv_2009-2013_2014-03-21.csv")
  # should set column 5 and 17 to character, but don't think I use those columns


# have length, horse power. those might change over years. will use last year, at most that will over-estimate how much gas/capacity a vessel has. Link it to yrdf by SVID

vs_sv <- ddply(sv, .(SVID), summarize, hp = HP[which.max(YEAR)], len = LEN[which.max(YEAR)], year = max(YEAR))

# now add in CG data
cg <- fread("/Users/efuller/1/CNH/Data/Catch/cg_2009-2013_2014-03-21.csv")

vs_cg <- ddply(cg, .(VID), summarize, hp = HP[which.max(PUBYR)], len = LEN[which.max(PUBYR)], grosswt = GROSSWT[which.max(PUBYR)], netwt = NETWT[which.max(PUBYR)], hp_astern = HP_MAIN_ASTERN[which.max(PUBYR)], breadth = BREADTH[which.max(PUBYR)], depth = DEPTH[which.max(PUBYR)], year = max(PUBYR))

# put together sv and cg, and if have measurements for both years, for hp and len, then use whichever is newer. if both the same age, use the bigger values. [AM HERE]

  # rename to keep seperate
  colnames(vs_sv) <- paste(colnames(vs_sv),"sv",sep="_")
  colnames(vs_cg) <- paste(colnames(vs_cg),"cg",sep="_")

  # merge two datasets
  vs <- merge(vs_sv, vs_cg, by.x = "SVID_sv", by.y = "VID_cg", all.x=TRUE,all.y=TRUE)

  # compare cg and sv derived lengths and horsepowers and years
  with(vs, plot(hp_sv, hp_cg,pch=19, col=alpha("black",.25), main = "hp",ylim=c(0,2000))); abline(0,1,lwd=2, col="purple4")
  with(vs, plot(len_sv, len_cg,pch=19, col=alpha("black",.25),cex=1, main='len',ylim=c(0,130),xlim=c(21,130))); abline(0,1,lwd=2, col="purple4")
  with(vs, plot(jitter(year_sv), jitter(year_cg),pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")
with(vs, plot(hp_sv, hp_astern_cg,pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")
with(vs, plot(hp_cg, hp_astern_cg,pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")


# maybe seems like most of the records in cg are later than sv. so will prioritize cg. 

final_vs <- data.frame(veid=vs$SVID_sv, hp = rep(NA,nrow(vs)), len = rep(NA,nrow(vs)), grosswt = rep(NA,nrow(vs)), netwt = rep(NA, nrow(vs)), breadth = rep(NA, nrow(vs)), depth = rep(NA, nrow(vs)))

final_vs$hp <- ifelse(!is.na(vs$hp_cg), vs$hp_cg, 
                      # if real value here for hp_cg, use it, else...
                      ifelse(!is.na(vs$hp_astern_cg), vs$hp_astern_cg, vs$hp_sv))
                      # if real value for hp_astern use it, else use sv (even if it's NA)

final_vs$len <- ifelse(!is.na(vs$len_cg), vs$len_cg, vs$len_sv)
final_vs$grosswt <- vs$grosswt_cg
final_vs$netwt <- vs$netwt_cg                
final_vs$breadth <- vs$breadth_cg
final_vs$depth <- vs$depth_cg

# merge vessel stats with yrdf[[2]]
new_df <- merge(yrdf[[2]], final_vs, by="veid", all.x = TRUE)

# add two new functions from help page to mess with pairs

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "slategrey",bor=FALSE)
}

# plot pairs
#pairs(new_df[,c("cv_revenue", "mean_simpson","hp","len","grosswt","netwt","breadth","depth")], upper.panel= panel.smooth, diag.panel = panel.hist, lower.panel=NULL,lwd=3, pch=19,cex=.75, col=alpha("slategray",.25))

nrow(new_df) == nrow(yrdf[[2]]) # should be TRUE

yrdf[[2]] <- new_df; rm(new_df);

#----
# adding mean latitude of landings
#----
tickets <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/tickets.RDS")
pcid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/pcid.csv",stringsAsFactors=FALSE)
pcid <- subset(pcid, Pcid %in% unique(tickets$pcid))

pcid$latitude <- c(39.224893,37.751957, 48.500226, 38.911100, 46.184623,35.175198, 38.313621,43.119615,37.859178, 48.689689,48.990874,37.902163,39.441044,42.050410, 45.884918, 43.349049, 41.749900,36.947737, 33.454366,44.810097, 40.805712, 47.998856,40.726245, 43.987470, 48.538127,42.405515,46.900394,46.012960, 34.141570, 48.369945,47.902987, 33.706400,46.299853,36.618916,36.806517,35.366324,48.370974, 44.625197, 45.701929,45.404521, 33.605090, 37.831454, 34.354413,NA,36.778984,33.178001,46.247297,41.834321,40.933045,33.646691, 47.063427,39.314516, 47.854287,42.739815,32.684806,37.597894,35.223022, 38.087555,NA,NA,34.147309,48.123373,45.206981,37.466655,37.923771,38.095665,34.387405,32.670935,47.601886, 48.073745,37.748742,47.216280, 37.856995,44.910513,33.701236,47.281443,45.485446,38.177928,48.112239,33.712004,41.054473,34.248640,43.666526,46.539371,44.422257,33.714138,46.888297)

trips <- unique(tickets[,c("trip_id","pcid","veid")])
# merge latitudes with tickets
tg <- merge(trips, pcid[,c("latitude","Pcid")], by.x = "pcid", by.y="Pcid", all.x=TRUE)

# only NAs are "other" ports for which I don't have a good idea
unique(tg$pcid[which(is.na(tg$latitude))])
# small proportion of total trips, but might be non-random in vessels
table(tg$pcid[which(is.na(tg$latitude))])/nrow(tg)
# leaving it for now though. 

mean_lat <- ddply(tg, .(veid), summarize, mean_landing = mean(latitude,na.rm=T))

new_df <- merge(yrdf[[2]], mean_lat, by = "veid",all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be true
yrdf[[2]] <- new_df; rm(new_df)
#----
# add port diversity index -- single port? lots of ports?
#----
library(reshape2)
melt_trips <- melt(trips, id.vars = "veid",measure.vars = "pcid")
cast_trips <- dcast(melt_trips, veid ~ value, fun.aggregate = length)
row.names(cast_trips) <- cast_trips$veid
cast_trips$veid <- NULL
library(vegan)
div_ports <- as.data.frame(diversity(cast_trips, index="simpson"))
colnames(div_ports) <- "simpsons_port_diversity"
div_ports$veid <- rownames(div_ports)
row.names(div_ports) <- NULL

new_df <- merge(yrdf[[2]], div_ports, all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be TRUE
yrdf[[2]] <- new_df; rm(new_df)
#----
# add processor diversity index -- single processor? lots of processors?
#----
processors <- unique(tickets[,c("veid","processorid","trip_id")])
processors$processorid[which(processors$processorid=="")] <- NA # a few empty ones, make them NA
melt_processors <- melt(processors, id.vars = "veid",measure.vars = "processorid")
cast_processors <- dcast(melt_processors, veid ~ value, fun.aggregate = length)
row.names(cast_processors) <- cast_processors$veid
cast_processors$veid <- NULL
div_processors <- as.data.frame(diversity(cast_processors, "simpson"))
colnames(div_processors) <- "simpsons_processor_diversity"
div_processors$veid <- rownames(div_processors)
row.names(div_processors) <- NULL

new_df <- merge(yrdf[[2]], div_processors, all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be TRUE
yrdf[[2]] <- new_df; rm(new_df)

# save yrdf update
saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf2.RDS")
