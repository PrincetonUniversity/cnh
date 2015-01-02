# function to classify years: 
rm(list=setdiff(ls(),"filtered_ftl"))
path <- "/tigress/efuller/classify_trips/2014-10-08/"

source("/tigress/efuller/classify_trips/2014-10-08/1_assign_trips.R")

libloc = "/tigress/efuller/R_packages"

library(reshape2, lib.loc = libloc); library(permute, lib.loc = libloc); library(plyr, lib.loc = libloc); library(dplyr, lib.loc = libloc);library(vegan, lib.loc = libloc);

args <- commandArgs(trailingOnly = TRUE) # if called from CL
if(length(args)==0){
  args = c("2010", "TLS", "2013") # place filler in case want to call from CL
}

ref_year <- args[1] # 2010 or 2012
grgroup <- args[2] # grgroup code from PacFin. Cannot be DRG
class_year <- args[3] # year of data to classify: 2009, 2011, 2012, 2013

# trips that have been filtered down to more common species 
# (so some species catch has been removed)
  if(!exists("filtered_ftl")){
    filtered_ftl <- readRDS("/tigress/efuller/classify_trips/2014-10-08/filtered_ftl.RDS")
  }

# try for one gear type
train <- read.csv(paste0(path,"metier_lists/",grgroup,ref_year,".csv"),
                          stringsAsFactors=F)
  train$X <- NULL
  train$year <- ref_year
  train$trip_id <- paste0(train$ftid, train$year)

# get new year to classify 
  year_choice = class_year
  gear_choice = grgroup
  classify <- unique(subset(filtered_ftl, grgroup==gear_choice & year==year_choice)$trip_id)

# if POT grgroup or MSC 2009 that's the classifying group, then will split the classified trips into 10 groups so we can handle it. 
if(grgroup == "POT" | grgroup == "MSC" & year_choice == 2009){
  cat("splitting classify up\n")
  length_df <- floor(length(classify)/10) # how many trips per group
  for(n in 1:10){
    start <- ifelse(n==1, 1, (n-1)*length_df+n)
    end <- start + length_df
    assign(paste0("c",n), classify[start:end])
  }
  c10 <- c10[!is.na(c10)]
  if(length(setdiff(c(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10), classify)) != 0) warning("split of classify got messed up")
  clist <- list(c1,c2,c3,c4,c5,c6,c7,c8,c9,c10)
  
  predicted_list <- lapply(clist, function(c_sub) assign_trips(classify_list = c_sub) )
  predicted_trips <- ldply(predicted_list, data.frame)
}else{
  # classify trips
    predicted_trips <- assign_trips()
}

# save output
saveRDS(predicted_trips, file=paste0(path,"predicted_metiers/",ref_year,"/",ref_year,"p",grgroup,class_year,".RDS"))
