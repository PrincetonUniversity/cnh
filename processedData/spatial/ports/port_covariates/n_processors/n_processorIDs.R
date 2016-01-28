# find number of processors at each port
rm(list=ls())
all_ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")

library(plyr)
num_procs <- ddply(tickets, .(pcid, year), summarize, num_procs = length(unique(processorid)))
num_procs_all <- ddply(num_procs, .(pcid), summarize, num_procs = mean(num_procs))

write.csv(num_procs_all, "/Users/efuller/1/CNH/processedData/spatial/ports/n_processorIDs.csv",row.names = FALSE)
# there's a lot of turnover from year to year. but will use average annual number of processors at a port
# also Princeton has 70 first recievers. what's up with that?