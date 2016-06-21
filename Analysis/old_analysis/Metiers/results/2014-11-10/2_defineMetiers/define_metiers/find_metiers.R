args <- commandArgs(trailingOnly = TRUE)
args[1] <- as.character(args[1])
args[2] <- as.numeric(args[2])

library_location <- "/tigress/efuller/R_packages/"

library(permute, lib.loc = library_location); library(reshape2, lib.loc = library_location)
library(plyr, lib.loc = library_location); library(vegan, lib.loc = library_location); library(igraph, lib.loc = library_location); library(dplyr, lib.loc = library_location)
source("helper_funs.R"); 

ftl <- readRDS("filtered_ftl.RDS")
just_gear <- select(ftl, ftid, grid)
just_gear <- unique(just_gear)
gear_types <- table(just_gear$ftid, just_gear$grid)
total_gear <- rowSums(gear_types)
extra_gear_trips <- names(total_gear)[which(total_gear > 1)]

ftl_trips <- subset(ftl, !(ftid %in% extra_gear_trips))

metier_trips <- find_metiers(tickets = ftl_trips, year = args[2], gear_group = args[1], message = "YES")

write.csv(metier_trips, file = paste0(args[1], args[2],".csv"))
