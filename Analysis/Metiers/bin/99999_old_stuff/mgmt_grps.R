# convert fish tickets to proportion of catch for each management category
# input: fish ticket csv file, species ID lookup table from pacfin
# output: tickets, prop_table which are the lookup table for fish ticket data and proportion of catch in each management category; dat_setup, dat_prop which are the ticket data after a PCA has been applied and the reduced to the number of principal components which retains 80% of the variance. 

# load required packages
rm(list=ls())
require(plyr); require(dplyr); require(data.table); require(reshape2); require(RColorBrewer); require(cluster)

# read in data
FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",stringsAsFactors=F)
colnames(FTL) <- tolower(colnames(FTL))

# read in look-up table for management groups
spid <- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/spid.csv",stringsAsFactors=F)

# list management groups
mgmt_grp <- dlply(spid, .(mgmt_grp))

# remove NA group
mgmt_grp <- mgmt_grp[2:9]

# add management group variable to fish tickets

m.vec <- rep(NA,nrow(FTL))

for(i in 1:length(mgmt_grp)){
  m.vec[FTL$spid %in% mgmt_grp[[i]]$SPID] = names(mgmt_grp[i])
}

tickets <- select(FTL, ftid, veid, year, spid, landed_wt, ppp, grgroup, grid, tdate,pcid)
tickets$mgmt_grp <- m.vec

# now look at amount caught per trip
by_trip <- data.table(tickets)
setkey(by_trip, ftid)
catch <- by_trip[, sum(landed_wt), by=c("ftid","mgmt_grp")]

total_catch <- dcast.data.table(catch, ftid ~ mgmt_grp, fun=sum)

# find proportion table
prop_table <- as.data.frame(total_catch)

# replace lbs with proportion of trip 
prop_table[,2:ncol(prop_table)] <- prop_table[,2:ncol(prop_table)]/rowSums(prop_table[,2:ncol(prop_table)])

# save output
save(prop_table, file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable.Rdata")
saveRDS(tickets, file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets.Rda") # couldn't get tickets to save other way

# do pca
pca_prop <- prcomp(prop_table[,2:ncol(prop_table)],scale=T)
npc <- length(which(summary(pca_prop)[[6]][3,]< .79))

# check to makes sure npc is greater than .8
while(summary(pca_prop)[[6]][3,][npc]<.8) npc = npc + 1

# retain principal components which retain > 80% of variation
dat_setup <- pca_prop$x[,1:npc]

# prepare for cluster analysis

# randomize rows to prevent any bias from order of fish tickets
# set up row index, so can translate back to original order
row <- seq(1:nrow(dat_setup))
dat_setup <- cbind(dat_setup,row)
dat_setup <- dat_setup[sample(nrow(dat_setup)),]
dat_prop <- dat_setup[,1:(ncol(dat_setup)-1)]

save(dat_setup,dat_prop, file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/datSetup_datProp.Rdata")
