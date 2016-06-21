# trying to figure out what happened with the HKL groups

# load data 
library(e1071)

agreement <- function (gear, year) {
  df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/2_defineMetiers/classify_trips/predicted_metiers/2010/2010p",gear, year,".RDS"))
  df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/2_defineMetiers/classify_trips/predicted_metiers/2012/2012p",gear, year,".RDS"))
  
  df10$predicted_metier <- paste0(df10$predicted_metier,"_10")
  df12$predicted_metier <- paste0(df12$predicted_metier,"_12")
  
  predicted_df <- merge(df10, df12, by = "trip_id")
  table(predicted_df$predicted_metier.x, predicted_df$predicted_metier.y)
}
# look at agreement

classAgreement(agreement("HKL",2009))$crand

# why is this?
# has to do with the way HKL_1_12 and HKL_1_10 split up trips. 

classAgreement(agreement("HKL",2009)[-1,][,-1])$crand

# HKL_1_12 puts together  HKL_2_10 (black rockfish other hook/line), HKL_3_10 (brown rockfish/gopher rockfish; pole), HKL_5_10 (lingcod; longline, other hook/line), and HKL_8_10 (vermillion rockfish; pole)
# HKL_1_10 puts together HKL_2_12 and HKL_5_12

# load tickets
tickets <- readRDS("code/1_cleaningData/filtered_ftl.RDS")

# look at 2009
HKL12 <- readRDS("code/2_defineMetiers/classify_trips/predicted_metiers/2012/2012pHKL2009.RDS")

mets12_2009 <- merge(tickets, HKL12)
mets12_2009$predicted_metier <- paste0(mets12_2009$predicted_metier,"_12")
colnames(mets12_2009)[ncol(mets12_2009)] <- "m2012"
HKL10 <- readRDS("code/2_defineMetiers/classify_trips/predicted_metiers/2010/2010pHKL2009.RDS")
mets <- merge(mets12_2009, HKL10, by = "trip_id")
colnames(mets)[ncol(mets)] <- "m2010"
mets$m2010 <- paste0(mets$m2010, "_10")

sort(table(hkl_1_12$spid))

with(subset(mets, m2010=="HKL_1_10"), sort(table(spid)))

# look at HKL_1_12
hkl_1_12 <- subset(mets, m2012=="HKL_1_12")
hkl_1_12_trips <- unique(hkl_1_12[,c("trip_id", "m2012","m2010")])

sort(table(hkl_1_12$spid))

with(subset(mets, m2010=="HKL_1_10"), sort(table(spid)))
# seems to have sablefish mostly. Is there a difference between the two big categories in 2012

# 2010 bagging: looks like both are sablefish-y. But HKL_2_12 way more so. 
par(mfrow=c(1,2))
with( subset(mets, m2010=="HKL_1_10" & m2012=="HKL_2_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.5, main = "HKL_2_12"))

with( subset(mets, m2010=="HKL_1_10" & m2012=="HKL_2_12"), barplot(sort(table(grid),decreasing=T), las=2,bor=F, cex.names=.5, main = "HKL_2_12"))


with(subset(mets, m2010=="HKL_1_10" & m2012=="HKL_5_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.5, main = "HKL_5_12"))
with(subset(mets, m2010=="HKL_1_10" & m2012=="HKL_5_12"), barplot(sort(table(grid),decreasing=T), las=2,bor=F, cex.names=.5, main = "HKL_5_12"))

# 2012
# HKL_1_12 puts together  HKL_2_10 (black rockfish other hook/line), HKL_3_10 (brown rockfish/gopher rockfish; pole), HKL_5_10 (lingcod; longline, other hook/line), and HKL_8_10 (vermillion rockfish; pole)
par(mfrow=c(2,2))
with( subset(mets, m2010=="HKL_2_10" & m2012=="HKL_1_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.75))

with(subset(mets, m2010=="HKL_3_10" & m2012=="HKL_1_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.75))

with(subset(mets, m2010=="HKL_5_10" & m2012=="HKL_1_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.75))

with(subset(mets, m2010=="HKL_8_10" & m2012=="HKL_1_12"), barplot(sort(table(modified),decreasing=T), las=2,bor=F, cex.names=.75))
# not obvious to me why these are being put together. 
library(plyr)

avg_catch <- function(met1, met2){
  foo <- subset(mets, m2010==met1 & m2012==met2)
  bar <- ddply(foo, .(modified), summarize, mean=mean(landed_wt))
  bar <- bar[order(bar$mean),]
  barplot(bar$mean, names.arg=as.character(bar$modified),las=2)}

avg_catch("HKL_8_10","HKL_1_12")
avg_catch("HKL_5_10","HKL_1_12")
avg_catch("HKL_3_10","HKL_1_12")
avg_catch("HKL_2_10","HKL_1_12")


# might have to do with nominal species names.. Maybe HKL 12 didn't get re-run with nominal..?

# or the year? Are most of these trips in 2010 or 2012?
# should plot each year, the number of each metier that was landed. 
# especially for HKL_3_10, HKL_8_10, and HKL_5_10. If there's not a lot of these in 2012, could explain why 2012 based classification doesn't have a defining class for them. 

# load HKL 2010 based classifiers. 
path10 = "code/2_defineMetiers/classify_trips/predicted_metiers/2010/"
files10 <- list.files(path=path10) # load 2010
hkl_files10 <- files10[grep("HKL",files10)]
predicted_hkl10 <- do.call(rbind, lapply(paste0(path10,hkl_files10), readRDS))
defined_hkl10 <- read.csv("code/2_defineMetiers/define_metiers/HKL2010.csv",stringsAsFactors=F)
defined_hkl10$X <- NULL
defined_hkl10$ftid <- paste0(defined_hkl10$ftid,"2010")
colnames(defined_hkl10)[1] <- "trip_id"
colnames(predicted_hkl10)[2] <- "metier"
hkl_trips10 <- rbind(predicted_hkl10,defined_hkl10)

hkl_trips10 <- merge(tickets, hkl_trips10, by = "trip_id")

just_trips10 <- unique(hkl_trips10[,c("trip_id","year","metier")])

met_year10 <- (with(just_trips10, table(metier, year)))

plot(2009:2013,met_year[which(rownames(met_year10)=="HKL_3"),],type='o',ylim=c(300,4400), col="indianred",pch=19)
lines(2009:2013,met_year[which(rownames(met_year10)=="HKL_8"),],type='o',pch=19, col="orange")
lines(2009:2013,met_year[which(rownames(met_year10)=="HKL_5"),],type='o', pch=19, col="slategray")
lines(2009:2013,met_year[which(rownames(met_year10)=="HKL_2"),],type='o',pch=19, col="purple")

# now 2012
path = "code/2_defineMetiers/classify_trips/predicted_metiers/2012/"
files <- list.files(path=path) # load 2012
hkl_files <- files[grep("HKL",files)]
predicted_hkl <- do.call(rbind, lapply(paste0(path,hkl_files), readRDS))
defined_hkl <- read.csv("code/2_defineMetiers/define_metiers/HKL2012.csv",stringsAsFactors=F)
defined_hkl$X <- NULL
defined_hkl$ftid <- paste0(defined_hkl$ftid,"2012")
colnames(defined_hkl)[1] <- "trip_id"
colnames(predicted_hkl)[2] <- "metier"
hkl_trips <- rbind(predicted_hkl,defined_hkl)

hkl_trips <- merge(tickets, hkl_trips, by = "trip_id")

just_trips <- unique(hkl_trips[,c("trip_id","year","metier")])


met_year <- (with(just_trips, table(metier, year)))

plot(2009:2013,met_year[which(rownames(met_year)=="HKL_2"),],type='o',ylim=c(300,5000), col="indianred",pch=19)
# lines(2009:2013,met_year[which(rownames(met_year)=="HKL_8"),],type='o',pch=19, col="orange")
lines(2009:2013,met_year[which(rownames(met_year)=="HKL_5"),],type='o', pch=19, col="slategray")
# lines(2009:2013,met_year[which(rownames(met_year)=="HKL_2"),],type='o',pch=19, col="purple")

