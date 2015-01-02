# author: Emma Fuller
# date: 2014-04-28
# goal: generate tripTables for metier-type clustering
rm(list=ls())
source("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/FTL_cp.R")

FTL <- read.csv("/Volumes/NOAA_Data/CNH/Data/Catch/FTL_2009-2013_2014-03-21.csv",as.is=TRUE)
spid_remove<- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/remove_spid.csv",as.is=TRUE)
cmplx_remove <- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/cmplx.csv",as.is=TRUE)
mgmt_remove <- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/mgmt_grp.csv",as.is=TRUE)
species_data <- read.csv("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-04-28/new_plan/input_data/spid.csv",as.is=TRUE)

#lb_tripTable <- FTL_cp(FTL,type="lbs",times=300,spid_remove,cmplx_remove, mgmt_remove)
#log_tripTable <- FTL_cp(FTL,type="log",times=300,spid_remove,cmplx_remove, mgmt_remove)
prop_tripTable <- FTL_cp(FTL,type="proportion",times=300,spid_remove,cmplx_remove, mgmt_remove)
#price_tripTable <- FTL_cp(FTL,type="price",times=300,spid_remove,cmplx_remove, mgmt_remove)

#save(lb_tripTable,file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-05-06/lb_tripTable_",Sys.Date(),".Rdata",sep=""))
#save(log_tripTable,file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-05-06/log_tripTable_",Sys.Date(),".Rdata",sep=""))
save(prop_tripTable,file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-05-17/prop_tripTable_",Sys.Date(),".Rdata",sep=""))
#save(price_tripTable,file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-05-06/price_tripTable_",Sys.Date(),".Rdata",sep=""))
