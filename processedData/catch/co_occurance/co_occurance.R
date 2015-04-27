# looking at overlap in participation: are metiers complementary or substitutable?
# looking at complementarity and substituablity. 
# for each metier pair, what number of trips occur in the same quarter or different quarters by vessel?

# load data, define quarters ----
tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/fisheries_participation_profiles/tickets_plus.RDS")
tickets$tdate <- as.Date(tickets$tdate, format = "%d-%b-%y")
# according to google: The four quarters that make up the year are: January, February and March (Q1); April, May and June (Q2); July, August and September (Q3); and October, November and December (Q4).

tickets$vyquarter <- ifelse(format(tickets$tdate,"%B") %in% c("January","February","March"), 
                          paste0("q1_",tickets$year,"_",tickets$drvid),
                   ifelse(format(tickets$tdate,"%B") %in% c("April","May","June"), 
                          paste0("q2_",tickets$year,"_",tickets$drvid), 
                   ifelse(format(tickets$tdate,"%B") %in% c("July","August","September"), 
                          paste0("q3_",tickets$year,"_",tickets$drvid),
                   ifelse(format(tickets$tdate,"%B") %in% c("October","November","December"), 
                          paste0("q4_",tickets$year,"_",tickets$drvid), 
                          NA))))
any(is.na(tickets$vyquarter)) # no NAs

trips <- unique(tickets[,c("trip_id","drvid","metier","vyquarter")])
nrow(trips) == length(unique(tickets$trip_id)) # should be TRUE

# make trip table of counts together ----
all_counts <- with(trips, table(metier, vyquarter))
all_mat <- as.matrix(all_counts)
all_x <- apply(all_mat, 2, function(x) as.numeric(x>0))
all_v <- all_x %*% t(all_x)
dimnames(all_v) <- list(names(all_counts[,1]), names(all_counts[,1]))

write.csv(all_v, "/Users/efuller/1/CNH/processedData/catch/co_occurance/co_occur.csv")

alone_all <- matrix(NA, nrow=nrow(all_v), ncol=ncol(all_v))
for(i in 1:nrow(alone_all)){
  alone_all[i,] <- diag(all_v) - all_v[i,]
}

normalized_all <- matrix(NA, nrow=nrow(all_v), ncol=ncol(all_v))
for(i in 1:nrow(normalized_all)){
  normalized_all[i,] <-(all_v[i,] - alone_all[i,])/diag(all_v)
}
dimnames(normalized_all) <- list(colnames(all_v),colnames(all_v))
diag(normalized_all) <- NA
levelplot(normalized_all,cex=.5)
hist(normalized_all)

# also maybe remove smalls?

smalls <- names(diag(all_v))[which(diag(all_v)<250)]
no_smalls <- normalized_all[-which(rownames(normalized_all)%in%smalls),]
no_smalls <- no_smalls[,-which(colnames(normalized_all)%in%smalls)]
levelplot(no_smalls)

no_smalls_pos <- all_v[-which(rownames(all_v)%in%smalls),]
no_smalls_pos <- no_smalls_pos[,-which(colnames(all_v)%in%smalls)]

# interpretation: it's the difference in fraction of total quarters that were in presence or absence of fishery. +1 means 100% in presence of another fishery. -1 is 100% by itself. 

# and it's the row's relationship to the column, i.e. HKL_1 is complementary with POT_1 but POT_1 is subtituable with HKL_1. Which is what the original data say too!

write.csv(all_v, "/Users/efuller/1/CNH/processedData/catch/co_occurance/together_counts.csv")
write.csv(alone_all, "/Users/efuller/1/CNH/processedData/catch/co_occurance/alone_counts.csv")
write.csv(normalized_all, "/Users/efuller/1/CNH/processedData/catch/co_occurance/normalized_counts.csv")
