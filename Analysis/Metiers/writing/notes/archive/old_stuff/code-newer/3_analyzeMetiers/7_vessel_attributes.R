# adding mean latitude of landings, horse power, and vessel length into yrdf dataset.

#----
# adding len/hp/etc.
#----
rm(list=ls())
yrdf <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf.RDS")
library(data.table); library(plyr); library(scales)
sv <- read.table("/Users/efuller/1/CNH/Data/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv",stringsAsFactors=F,skip=4, sep="|",fill=TRUE,quote="")

# still need column names
library(stringr); library(plyr)
f <- readLines("/Users/efuller/1/CNH/Data/Catch/vessel_registration/SV_2009-2013_woc_141210_two.csv")
c.names <- unlist(str_split(f[3],c(", ")))
c.names <- c(unlist(str_split(c.names[1],": ")),c.names)
c.names <- c.names[-1]
c.names <- c.names[-2]
real_names <- c(c.names[1:11], "ulid", c.names[12:17]) # from Brad, see email

colnames(sv) <- real_names
# some colnames have hyphens, replace with periods
colnames(sv) <- gsub("-",".",colnames(sv))

# remove the "X entries selected" metadata
sv <- subset(sv, idtype!="")

# reset year column to be numeric
sv$year <- as.numeric(sv$year)
# find maximum year entries 
vs_sv <- ddply(sv, .(svid), summarize, len = length[which.max(year)], weight = weight[which.max(year)], hp = horsepower[which.max(year)], year = max(year))
# check
length(unique(vs_sv$svid)) == length(unique(sv$svid)) # should be TRUE
# any duplicates?
any(duplicated(vs_sv$svid)) # FALSE, good

#----
# cg
#----

cg09_12 <- read.table("/Users/efuller/1/CNH/Data/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv",stringsAsFactors=F, sep="|",quote="",fill=TRUE,skip=3)
g <- readLines("/Users/efuller/1/CNH/Data/Catch/vessel_registration/CG_2009-2012_woc_141210_three.csv")
c.names <- unlist(str_split(g[3],c(", ")))
c.names <- c(unlist(str_split(c.names[1],": ")),c.names)
c.names <- c.names[-1]
c.names <- c.names[-2]
colnames(cg09_12) <- c.names

# problem, some vessel IDs are not numeric, are CA or something else. 

# find all non-numeric entries in that column
# probs <- cg09_12[-grep("^[0-9]", cg09_12$vessel_id),]
# unique(probs$vessel_id)
# # just empty or CA. 

# of that data, there is just one that is useful, the one with CA. so in main data-set fix that manually but doesnt' have a year, so dropping it. 

cg09_12[which(cg09_12$vessel_id=="CA"),"vessel_id"] <- cg09_12[which(cg09_12$vessel_id=="CA"),"pubyr"]
cg09_12[which(cg09_12$vessel_id=="92038"),"pubyr"] <- NA

# now remove any cg that has a vessel_id==""
cg09_12 <- subset(cg09_12, vessel_id!="")

# remove anything that doesn't have a year
cg09_12 <- subset(cg09_12, !is.na(pubyr) & pubyr!=7)

# now load cg_2013
cg13 <- read.table("/Users/efuller/1/CNH/Data/Catch/vessel_registration/CG_2013_woc_141210_three.csv",stringsAsFactors=F, sep="|",quote="",fill=TRUE,skip=3)
#same columns, rbind to cg
colnames(cg13) <- colnames(cg09_12)
cg <- rbind(cg09_12, cg13)

# remove any records where cg == ""
cg <- subset(cg, vessel_id!="")

# remove any where pubyr == NA
cg <- subset(cg, !is.na(pubyr))

# or when pubyr is not 2009-2013
cg <- subset(cg, pubyr %in% 2009:2013)

vs_cg <- ddply(cg, .(vessel_id), summarize, hp = horsepower[which.max(pubyr)], len = length[which.max(pubyr)], gross_wt = gross_weight[which.max(pubyr)], net_wt = net_weight[which.max(pubyr)], hp_astern = hp_main_astern[which.max(pubyr)], breadth = breadth[which.max(pubyr)], depth = depth[which.max(pubyr)], year = max(pubyr))

# check to make sure all vessels still there
length(unique(vs_cg$vessel_id)) == length(unique(cg$vessel_id)) # should be TRUE
# any duplicates?
any(duplicated(vs_cg$vessel_id)) # FALSE, good
#----
# combine datasets
#----
colnames(vs_sv) <- paste(colnames(vs_sv),"sv",sep="_")
colnames(vs_cg) <- paste(colnames(vs_cg),"cg",sep="_")

# merge two datasets
vs <- merge(vs_sv, vs_cg, by.x = "svid_sv", by.y = "vessel_id_cg", all.x=TRUE,all.y=TRUE)

# compare cg and sv derived lengths and horsepowers and years
library(scales)
with(vs, plot(hp_sv, hp_cg,pch=19, col=alpha("black",.25), main = "hp",ylim=c(0,2000))); abline(0,1,lwd=2, col="purple4")
with(vs, plot(len_sv, len_cg,pch=19, col=alpha("black",.25),cex=1, main='len',ylim=c(0,130),xlim=c(21,130))); abline(0,1,lwd=2, col="purple4")
with(vs, plot(jitter(year_sv), jitter(year_cg),pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")
with(vs, plot(hp_sv, hp_astern_cg,pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")
with(vs, plot(hp_cg, hp_astern_cg,pch=19, col=alpha("black",.25),cex=1)); abline(0,1,lwd=2, col="purple4")

# maybe seems like most of the records in cg are later than sv. so will prioritize cg. 
final_vs <- data.frame(drvid=vs$svid_sv, hp = rep(NA,nrow(vs)), len = rep(NA,nrow(vs)), grosswt = rep(NA,nrow(vs)), netwt = rep(NA, nrow(vs)), breadth = rep(NA, nrow(vs)), depth = rep(NA, nrow(vs)),stringsAsFactors=F)

final_vs$hp <- ifelse(!is.na(vs$hp_cg), vs$hp_cg, 
                      # if real value here for hp_cg, use it, else...
                      ifelse(!is.na(vs$hp_astern_cg), vs$hp_astern_cg, vs$hp_sv))
# if real value for hp_astern use it, else use sv (even if it's NA)

final_vs$len <- ifelse(!is.na(vs$len_cg), vs$len_cg, vs$len_sv)
final_vs$grosswt <- vs$gross_wt_cg
final_vs$netwt <- vs$net_wt_cg                
final_vs$breadth <- vs$breadth_cg
final_vs$depth <- vs$depth_cg

# merge vessel stats with yrdf[[2]]
new_df <- merge(yrdf[[2]], final_vs, by="drvid", all.x = TRUE)

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

trips <- unique(tickets[,c("trip_id","pcid","drvid")])
# merge latitudes with tickets
tg <- merge(trips, pcid[,c("latitude","Pcid")], by.x = "pcid", by.y="Pcid", all.x=TRUE)

# only NAs are "other" ports for which I don't have a good idea
unique(tg$pcid[which(is.na(tg$latitude))])
# small proportion of total trips, but might be non-random in vessels
table(tg$pcid[which(is.na(tg$latitude))])/nrow(tg)
# leaving it for now though. 

mean_lat <- ddply(tg, .(drvid), summarize, mean_landing = mean(latitude,na.rm=T))

new_df <- merge(yrdf[[2]], mean_lat, by = "drvid",all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be true
yrdf[[2]] <- new_df; rm(new_df)
#----
# add port diversity index -- single port? lots of ports?
#----
library(reshape2)
melt_trips <- melt(trips, id.vars = "drvid",measure.vars = "pcid")
cast_trips <- dcast(melt_trips, drvid ~ value, fun.aggregate = length)
row.names(cast_trips) <- cast_trips$drvid
cast_trips$drvid <- NULL
library(vegan)
div_ports <- as.data.frame(diversity(cast_trips, index="simpson"))
colnames(div_ports) <- "simpsons_port_diversity"
div_ports$drvid <- rownames(div_ports)
row.names(div_ports) <- NULL

new_df <- merge(yrdf[[2]], div_ports, all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be TRUE
yrdf[[2]] <- new_df; rm(new_df)
#----
# add processor diversity index -- single processor? lots of processors?
#----
processors <- unique(tickets[,c("drvid","processorid","trip_id")])
processors$processorid[which(processors$processorid=="")] <- NA # a few empty ones, make them NA
melt_processors <- melt(processors, id.vars = "drvid",measure.vars = "processorid")
cast_processors <- dcast(melt_processors, drvid ~ value, fun.aggregate = length)
row.names(cast_processors) <- cast_processors$drvid
cast_processors$drvid <- NULL
div_processors <- as.data.frame(diversity(cast_processors, "simpson"))
colnames(div_processors) <- "simpsons_processor_diversity"
div_processors$drvid <- rownames(div_processors)
row.names(div_processors) <- NULL

new_df <- merge(yrdf[[2]], div_processors, all.x=TRUE)
nrow(new_df)==nrow(yrdf[[2]]) # should be TRUE
yrdf[[2]] <- new_df; rm(new_df)

#----
# Trophic level of catch!
#----

#----

# save yrdf update
saveRDS(yrdf, "/Users/efuller/1/CNH/Analysis/Metiers/writing/code/3_analyzeMetiers/yrdf2.RDS")
