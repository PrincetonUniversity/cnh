# comparing ARI with modified versus spid

library(e1071)

agreement <- function (gear, year, species_def) {
  if(species_def == "mod"){
    df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/2010/2010p",gear, year,".RDS"))
    df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/2012/2012p",gear, year,".RDS"))
  }else if(species_def=="spid"){
    df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/spid/2010/2010p",gear, year,".RDS"))
    df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/spid/2012/2012p",gear, year,".RDS"))    
  } else{
    warning("species_def needs to be 'mod' or 'spid'")
  }
  
  df10$predicted_metier <- paste0(df10$predicted_metier,"_10")
  df12$predicted_metier <- paste0(df12$predicted_metier,"_12")
  
  predicted_df <- merge(df10, df12, by = "trip_id")
  table(predicted_df$predicted_metier.x, predicted_df$predicted_metier.y)
}
# look at agreement

years = c(2009,2011,2013)
mod_ari <- rep(NA,3)
for(i in 1:length(years)){
  mod_ari[i] <- classAgreement(agreement("HKL",years[i],"mod"))$crand
}

spid_ari <- rep(NA,3)
for(i in 1:length(years)){
  spid_ari[i] <- classAgreement(agreement("HKL",years[i],"spid"))$crand
}

# confirms it. something about going to modified screws things up. well shoot. at least it's reproducible

# what do I want to know.. 

# comparing within years spid to modified

agreement_defs <- function(year_base, year_class){
  df_mod <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/",year_base,"/",year_base,"pHKL",year_class,".RDS"))
  df_spid <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/spid/",year_base,"/",year_base,"pHKL",year_class,".RDS"))
  
  df_mod$predicted_metier <- paste0(df_mod$predicted_metier,"_mod")
  df_spid$predicted_metier <- paste0(df_spid$predicted_metier,"_spid")
  
  predicted_df <- merge(df_mod, df_spid, by = "trip_id")
  table(predicted_df$predicted_metier.x, predicted_df$predicted_metier.y)
}

defs_10 <- rep(NA,3)
for(i in 1:length(years)){
  defs_10[i] <- classAgreement(agreement_defs(2010, years[i]))$crand
}

defs_12 <- rep(NA,3)
for(i in 1:length(years)){
  defs_12[i] <- classAgreement(agreement_defs(2012, years[i]))$crand
}

which(rownames(agreement_defs(2012,2013))=="HKL_1_mod")
classAgreement(agreement_defs(2012,2013)[-1,])$crand
# really the problem of HKL_mod_1 what is it combining?

# load in ticket data
ftl <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/filtered_ftl.RDS")
mod12_13 <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/2012/2012pHKL2013.RDS")
spid12_13 <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/spid/2012/2012pHKL2013.RDS")

hkl2013 <- merge(ftl, mod12_13, by = "trip_id")
colnames(hkl2013)[ncol(hkl2013)] <- "mod_metier"
hkl2013 <- merge(hkl2013, spid12_13, by = "trip_id")
colnames(hkl2013)[ncol(hkl2013)] <- "spid_metier"

#just ones misclassified
missed <- hkl2013[which(hkl2013$mod_metier=="HKL_1"),]
# is it that the spids are different than the modified?

different_spid <- missed[which(missed$spid!=missed$modified),]
sort(with(different_spid, table(mod_metier, spid_metier))[1,],decreasing = TRUE)
# nope, evenly split

# is it the vermillion issue?
verm <- missed[which(missed$spid=="VRM1"),]
sort(with(verm, table(mod_metier, spid_metier))[1,],decreasing = TRUE)
# seems like vermillion trips are mostly spid HKL_3

# try to characterize spid HKL_3 and spid HKL_1
library(plyr)
selective_missed <- missed[which(missed$spid_metier %in% c("HKL_1","HKL_3")),]
confused_spids <- ddply(selective_missed, .(spid_metier, spid), summarize, mean_spid = mean(landed_wt), sd_spid = sd(landed_wt))
# order spid as factors by HKL_3 abundance of spids
order_spids <- confused_spids[which(confused_spids$spid_metier=="HKL_3"),]
order_spids <- order_spids[order(order_spids$mean_spid,decreasing=T),]

confused_spids$spid <- factor(confused_spids$spid, levels = order_spids$spid)

library(ggplot2)
ggplot(confused_spids, aes(x = spid, y = mean_spid, fill = factor(spid_metier))) + geom_bar(stat="identity",position="dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# trying mods
confused_mods <- ddply(selective_missed, .(spid_metier, modified), summarize, mean_mod = mean(landed_wt), sd_mod = sd(landed_wt))
# order spid as factors by HKL_3 abundance of spids
order_mods <- confused_mods[which(confused_mods$spid_metier=="HKL_1"),]
order_mods <- order_mods[order(order_mods$mean_mod,decreasing=T),]

confused_mods$modified <- factor(confused_mods$modified, levels = order_mods$modified)

library(ggplot2)
ggplot(confused_mods, aes(x = modified, y = mean_mod, fill = factor(spid_metier))) + geom_bar(stat="identity",position="dodge") + theme(axis.text.x = element_text(angle = 45, hjust = 1))



agreement_defs(2012,2013)

# feeling confused.. what I want to know is which species were changed the most by switching from spid to modified. and then maybe which trips. and if those trips are mostly HKL 3, that perhaps that explains it..?

# so want to find the number of species that changed
changed_spids <- ddply(selective_missed, .(trip_id), summarize, ids = length(which(spid!=modified))/length(spid), spid_met = unique(spid_metier))
plot(changed_spids$ids, changed_spids$spid_met)

logit.R <- glm (changed_spids$spid_met ~ changed_spids$ids, family=binomial(link="logit"))

# meh, don't really understand.. not the right direction.


# next steps: characterize the crap out of spid-based HKL_3 and HKL_1;
# how big are average catches (plot the distribution)
# what spids are caught (top 4?) and what are their distributions?
# what modifieds are caught (top 4?) and what are their distributions?
selective_missed$spid_metier <- as.character(selective_missed$spid_metier)

par(mfrow=c(2,1),mai=c(.65,0,0.1,0))
barplot(with(selective_missed, table(spid_metier, spid)), legend=TRUE, las=2, cex.names=.5)
barplot(with(selective_missed, table(spid_metier, modified)), las=2, cex.names=.5)
# think I'm cracking it! - nominal lingcod was primarily caught in HKL_3, while reg lingcod was in HKL_1. nominal cabezon was in HKL_3, but reg cabezon was in HKL_1

# are those that do cbzn from a particular set of ports?
cbzn <- selective_missed[which(selective_missed$spid=="CBZN"),]
table(cbzn$pcid) # mostly from port orford
cbz1 <- selective_missed[which(selective_missed$spid=="CBZ1"),]
table(cbz1$pcid) # almost all california (all?)

# what about ling?
lcod <- selective_missed[which(selective_missed$spid=="LCOD"),]
table(lcod$pcid) # again port orford
lcd1 <- selective_missed[which(selective_missed$spid=="LCD1"),]
table(lcd1$pcid) # more all over the place

# this suggests that by going to modified HKL_1 and HKL_3 are becoming the same... but this doesn't explain why the years make a difference.. 

# probably need to repeat this analysis for 2010 and see which species are getting put together? specifically for 2013

mod10_13 <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/2010/2010pHKL2013.RDS")
spid10_13 <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/spid/2010/2010pHKL2013.RDS")

hkl2013 <- merge(ftl, mod10_13, by = "trip_id")
colnames(hkl2013)[ncol(hkl2013)] <- "mod_metier"
hkl2013 <- merge(hkl2013, spid10_13, by = "trip_id")
colnames(hkl2013)[ncol(hkl2013)] <- "spid_metier"

# looks like HKL_1_mod puts together HKL_1_spid and HKL_5_spid and HKL_3_spid puts together HKL_3_mod and HKL_5_mod. Weird. Removing those we still get not great matching
classAgreement(agreement_defs(2010,2013))$crand
classAgreement(agreement_defs(2010,2013)[,-19][-1,])$crand
# removing also HKL_2_spid which puts together HKL_2_mod and HKL_5_mod
classAgreement(agreement_defs(2010,2013)[,-c(12,19)][-1,])$crand
# that goes higher. 

# try HKL_1_mod first 

#just ones misclassified
missed <- hkl2013[which(hkl2013$mod_metier=="HKL_1"),]
# is it that the spids are different than the modified?

different_spid <- missed[which(missed$spid!=missed$modified),]
sort(with(different_spid, table(mod_metier, spid_metier))[1,],decreasing = TRUE)
# nope, evenly split

# looks like it might have to do with longspine thornyheads. only port orford lists as LSP1, gets modified to LSPN and thus different metiers
head(different_spid[which(different_spid$spid_metier=="HKL_5"),],20)
head(different_spid[which(different_spid$spid_metier=="HKL_1"),],20)
# looks like sspn + darkblotched is fine for HKL_1 but the sspn+lspn was HKL_5. not clear to me why these were seperated for spid metiers and not for mod_metiers

sort(table(different_spid[which(different_spid$spid_metier=="HKL_5"),]$spid),decreasing=T)
sort(table(different_spid[which(different_spid$spid_metier=="HKL_1"),]$spid),decreasing=T)

# looks like HKL_1_spid has way more species. but both are mostly longline. 

# try to characterize spid HKL_5 and spid HKL_1
library(plyr)
selective_missed <- missed[which(missed$spid_metier %in% c("HKL_1","HKL_5")),]
confused_spids <- ddply(selective_missed, .(spid_metier, spid), summarize, mean_spid = mean(landed_wt), sd_spid = sd(landed_wt))

# trying mods
confused_mods <- ddply(selective_missed, .(spid_metier, modified), summarize, mean_mod = mean(landed_wt), sd_mod = sd(landed_wt))

selective_missed$spid_metier <- as.character(selective_missed$spid_metier)

par(mfrow=c(2,1),mai=c(.65,0,0.1,0))
barplot(with(selective_missed, table(spid_metier, spid)), legend=TRUE, las=2, cex.names=.5)
barplot(with(selective_missed, table(spid_metier, modified)), las=2, cex.names=.5)
# no real difference between these. all species ids have been shifted from nominal to markets. not clear why these put together. 

# will try HKL_3_spid which puts together HKL_3_mod and HKL_5_mod

#just ones misclassified
missed <- hkl2013[which(hkl2013$spid_metier=="HKL_3"),]
# is it that the spids are different than the modified?

different_spid <- missed[which(missed$spid!=missed$modified),]

head(different_spid[which(different_spid$mod_metier=="HKL_5"),],20)
head(different_spid[which(different_spid$mod_metier=="HKL_3"),],20)

sort(table(different_spid[which(different_spid$mod_metier=="HKL_5"),]$spid),decreasing=T)
sort(table(different_spid[which(different_spid$mod_metier=="HKL_3"),]$spid),decreasing=T)

# these are hard to tell apart

# try to characterize spid HKL_5 and spid HKL_1
selective_missed <- missed[which(missed$mod_metier %in% c("HKL_3","HKL_5")),]
confused_spids <- ddply(selective_missed, .(mod_metier, spid), summarize, mean_spid = mean(landed_wt), sd_spid = sd(landed_wt))

# trying mods
confused_mods <- ddply(selective_missed, .(mod_metier, modified), summarize, mean_mod = mean(landed_wt), sd_mod = sd(landed_wt))

selective_missed$mod_metier <- as.character(selective_missed$mod_metier)

par(mfrow=c(2,1),mai=c(.65,0,0.1,0))
barplot(with(selective_missed, table(mod_metier, spid)), legend=TRUE, las=2, cex.names=.5)
barplot(with(selective_missed, table(mod_metier, modified)), las=2, cex.names=.5)
# no real difference between these. all species ids have been shifted from nominal to markets. not clear why these put together. 


# will try HKL_2_spid which puts together HKL_2_mod and HKL_5_mod

#just ones misclassified
missed <- hkl2013[which(hkl2013$spid_metier=="HKL_2"),]
# is it that the spids are different than the modified?

different_spid <- missed[which(missed$spid!=missed$modified),]

head(different_spid[which(different_spid$mod_metier=="HKL_5"),],20)
head(different_spid[which(different_spid$mod_metier=="HKL_2"),],20)

sort(table(different_spid[which(different_spid$mod_metier=="HKL_5"),]$spid),decreasing=T)
sort(table(different_spid[which(different_spid$mod_metier=="HKL_2"),]$spid),decreasing=T)

# these are hard to tell apart

# try to characterize spid HKL_5 and spid HKL_1
selective_missed <- missed[which(missed$mod_metier %in% c("HKL_2","HKL_5")),]


selective_missed$mod_metier <- as.character(selective_missed$mod_metier)

par(mfrow=c(2,1),mai=c(.65,0,0.1,0))
barplot(with(selective_missed, table(mod_metier, spid)), legend=TRUE, las=2, cex.names=.5)
barplot(with(selective_missed, table(mod_metier, modified)), las=2, cex.names=.5)
# no real difference between these. all species ids have been shifted from nominal to markets. not clear why these put together. 

# but the mods look much worse. these seem like they should be the same fishery. 

# should do 2010 and 2012. 
# 2010 clustered
hkl2010_mod <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/define_mets/modified_defs/HKL2010.csv")
hkl2010_mod$X <- NULL
colnames(hkl2010_mod)[2] <- "c10"
hkl2010_mod$ftid <- paste0(hkl2010_mod$ftid,"2010")
colnames(hkl2010_mod)[1] <- "trip_id"

# 2012 predicted 2010
hkl2012_mod <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/classify_mets/modified/2012/2012pHKL2010.RDS")
colnames(hkl2012_mod)[2] <- "c12"

predicted <- merge(hkl2010_mod, hkl2012_mod,by="trip_id")
with(predicted, table(c10, c12))
classAgreement(with(predicted, table(c10, c12)))$crand

# bad ones are HKL_1_12
classAgreement(with(predicted, table(c10, c12))[,-1])$crand
# and HKL_1_10
classAgreement(with(predicted, table(c10, c12))[,-1][-1,])$crand

# HKL_1_10 puts HKL_2_12 and HKL_5_12 together -- I think this is a correct call. 
# HKL_1_12 puts HKL_2_10 and HKL_3_10 and HKL_5_10 and HKL_8_10 too

missed <- merge(ftl, predicted, by = "trip_id")
twelve_one <- missed[which(missed$c12=="HKL_1"),]
twelve_one$c10 <- as.character(twelve_one$c10)
paint = brewer.pal(4, "Dark2")
with(twelve_one[which(twelve_one$c10=="HKL_2" | twelve_one$c10=="HKL_3" | twelve_one$c10=="HKL_5"  | twelve_one$c10=="HKL_8"),], barplot(table(c10, spid), legend=TRUE, cex.names=.65, las=2, args.legend=list(bty="n",ncol=5, cex = 1), col = paint,bor=paint))
with(twelve_one[which(twelve_one$c10=="HKL_2" | twelve_one$c10=="HKL_3" | twelve_one$c10=="HKL_5"  | twelve_one$c10=="HKL_8"),], barplot(table(c10, modified), legend=TRUE, cex.names=.65, las=2, args.legend=list(bty="n",ncol=5, cex = 1), col = paint,bor=paint))

# why does modified 2010 break apart HKL_2_10 and HKL_3_10. 
two_three <- missed[which(missed$c10 == "HKL_2" | missed$c10 == "HKL_3"),]
two_three$c10 <- as.character(two_three$c10)
with(two_three, barplot(table(c10, modified), legend = TRUE, las=2, cex.names=.65))

# modified hkl_2 and hkl_3 from 2010 both look like they are pretty similar. not clear why they're broken apart by clustering. 

missed <- missed[which(missed$c10 == "HKL_1"),]
missed <- missed[which(missed$c12 =="HKL_2" | missed$c12 == "HKL_5"),]
missed$c10 <- as.character(missed$c10)
missed$c12 <- as.character(missed$c12)
with(missed, barplot(table(c12, spid), las=2, cex.names=.65))
with(missed, barplot(table(c12, modified),legend=TRUE, cex.names=.65, las=2))

missed12 <- merge(ftl, predicted, by = "trip_id")
missed12 <- missed12[which(missed12$c12 == "HKL_1"),]
missed12 <- missed12[which(missed12$c10 =="HKL_2" | missed12$c10 == "HKL_5" | missed12$c10 == "HKL_3" | missed12$c10 == "HKL_8"),]
missed12$c12 <- as.character(missed12$c12)
missed12$c10 <- as.character(missed12$c10)
with(missed12, barplot(table(c10, spid), las=2, cex.names=.65, col = c("blue","orange","purple","red"),bor=c("blue","orange","purple","red")))
with(missed12, barplot(table(c10, modified),legend=TRUE, cex.names=.65, las=2, col = c("blue","orange","purple","red"),bor=c("blue","orange","purple","red")))

# seems like lingcod and cabezon are the problems here. both are classified into different 2010 clusters based on spid, but obviously not on modified ids. but still not clear why this breaks the association between 2010 and 2012 modified... 

# where does HKL_1_modified go in HKL_spid?'
mod_12 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/define_mets/modified_defs/HKL2012.csv", stringsAsFactors=FALSE)
mod_12$X <- NULL
colnames(mod_12)[2] <- "mod"
spid_12 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/define_mets/spid_dfs//HKL2012.csv",stringsAsFactors=FALSE)
spid_12$X <- NULL
colnames(spid_12)[2] <- "spid"

in_year <- merge(mod_12, spid_12, by = "ftid")
with(in_year, table(mod, spid))
classAgreement(with(in_year, table(mod, spid)))$crand
# it's the HKL_1_mod that throws together HKL_1_spid and HKL_3_spid 
classAgreement(with(in_year, table(mod, spid))[-1,])$crand

in_year$ftid <- paste0(in_year$ftid, "2012")
colnames(in_year)[1] <- "trip_id"

iy_tickets <- merge(in_year, ftl, by = "trip_id")
iy_tickets <- iy_tickets[which(iy_tickets$mod=="HKL_1"),]
iy_tickets <- iy_tickets[which(iy_tickets$spid.x=="HKL_1" | iy_tickets$spid.x=="HKL_3"),]

barplot(table(iy_tickets$spid.x, iy_tickets$spid.y), las=2, cex.names=.65,legend=TRUE)
barplot(table(iy_tickets$spid.x, iy_tickets$modified), las=2, cex.names=.65,legend=F)

# it's the HKL_1_mod that throws together HKL_1_spid and HKL_3_spid because of cabezon and lingcod. so seems fine. 
# what about 2010?
mod_10 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/define_mets/modified_defs/HKL2010.csv", stringsAsFactors=FALSE)
mod_10$X <- NULL
colnames(mod_10)[2] <- "mod"
spid_10 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-06/define_mets/spid_dfs//HKL2010.csv",stringsAsFactors=FALSE)
spid_10$X <- NULL
colnames(spid_10)[2] <- "spid"

in_year <- merge(mod_10, spid_10, by = "ftid")
with(in_year, table(mod, spid))
classAgreement(with(in_year, table(mod, spid)))$crand
# it's the HKL_1_mod that throws together HKL_1_spid and HKL_5_spid and HKL_3_spid puts together HKL_3_mod and HKL_5_mod and HKL_2_spid puts together HKL_2_mod and HKL_5_mod.
# so HKL_5_mod gets split between HKL_2_spid and HKL_3_spid
classAgreement(with(in_year, table(mod, spid))[-1,][,-c(12,23)])$crand

in_year$ftid <- paste0(in_year$ftid, "2010")
colnames(in_year)[1] <- "trip_id"

iy_tickets <- merge(in_year, ftl, by = "trip_id")

# what is in HKL_5_mod that is confusing?
library(RColorBrewer)
paint = brewer.pal(6,"Spectral")
barplot(with(iy_tickets[which(iy_tickets$mod=="HKL_5"),], table(spid.x, spid.y)),legend=TRUE,las=2, cex.names=.65,col=paint, main="mod colored by spid_def")
barplot(with(iy_tickets[which(iy_tickets$mod=="HKL_5"),], table(spid.x, modified)),cex.names=.65, las=2, col=paint)

paint = brewer.pal(4,"Spectral")
# why does HKL_3 and HKL_2 get put with HKL_5 mod. makes sense why $HKL_5_mod gets condensed.
barplot(with(iy_tickets[which(iy_tickets$spid.x=="HKL_3"),], table(mod, spid.y)),legend=TRUE,las=2, cex.names=.65, main="mod colored by spid_def",col=paint,args.legend=list(ncol=2))
barplot(with(iy_tickets[which(iy_tickets$spid.x=="HKL_3"),], table(mod, modified)),cex.names=.65, las=2,col=paint)

paint = brewer.pal(5,"Spectral")
par(mfrow=c(2,1))
barplot(with(iy_tickets[which(iy_tickets$spid.x=="HKL_2"),], table(mod, spid.y)),legend=T,las=2, cex.names=.65,col=paint,args.legend=list(ncol=1,bty="n"))
barplot(with(iy_tickets[which(iy_tickets$spid.x=="HKL_2"),], table(mod, modified)),cex.names=.65, las=2,col=paint)

# all this might be solved by taking major gears and using those as ways to split up species..

# gear_types
gears <- unique(select(ftl, trip_id, grid, grgroup, year))
gears$grid <- factor(gears$grid)
gears$grgroup <- factor(gears$grgroup)
with(gears, sort(table(grid),decreasing=TRUE))
with(gears, sort(table(grgroup), decreasing=TRUE))

# think i should drop people who make less than $5000 and see what kind of gear distribution I get.

annual_rev <- ddply(ftl, .(veid, year), summarize, revenue = sum(ppp*landed_wt))
annual_rev <- annual_rev[-which(annual_rev$veid=="0"),]
mean_rev <- ddply(annual_rev, .(veid), summarize, mean_rev = mean(revenue))

# about a third of vessels have a mean revenue < 5000
length(which(mean_rev$mean_rev<5000))/nrow(mean_rev)

primary <- mean_rev[which(mean_rev$mean_rev>=5000),]
prim_ves <- subset(ftl, veid %in% primary$veid)
prim_gear <- unique(select(prim_ves, trip_id, grid, grgroup, year))


round(with(prim_gear, sort(table(grid),decreasing=TRUE))*100/nrow(prim_gear),2)
round(with(gears, sort(table(grid),decreasing=TRUE))*100/nrow(gears),2)
round(with(prim_gear[which(prim_gear$grgroup=="HKL"),], table(grid))*100/nrow(prim_gear[which(prim_gear$grgroup=="HKL"),]),2)
# jig and vhl make up small proportion of total hook and line trips. how much money are VHL and JIG vessels making? and do they do anything but that?

jig <- subset(prim_gear, grid=="JIG" )
jig_rev <- subset(prim_ves, trip_id %in% jig$trip_id)
