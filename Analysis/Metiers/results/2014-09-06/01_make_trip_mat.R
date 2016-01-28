# create trip matrix

# load fish tickets
ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=F)

# let's subset to Astoria tickets from 2012
astoria_trips <- subset(ftl, pcid=="AST" & year=="2012", select=c("spid", "ftid","landed_wt"))

# select relevant data, construct trip matrix
melt_astoria <- melt(astoria_trips, variables = c("spid","ftid"), measure.vars = "landed_wt")
cast_astoria <- dcast(melt_astoria, ftid ~ spid, fun.aggregate = sum)
rownames(cast_astoria) <- cast_astoria$ftid
cast_astoria$ftid <-NULL

headtrip_mat <- as.matrix(cast_astoria)

# remove species that show up in fewer than 1% of the trips
rare_bar <- round(nrow(trip_mat)*.01) 

count_occurances <- apply(trip_mat, 2, function(x) length(which(x>0)))

# this drops a number of species, will be different for a bigger dataset
trip_majority <- trip_mat[,-which(count_occurances < rare_bar)]

# check to makesure each trip has at least something
which(rowSums(trip_majority)==0)

# which trips?
trip_mat[which(rowSums(trip_majority)==0), ]
# mostly hagfish, but some coho trips and one rockfish trip. fine for now, this is a test, but won't want to loose them in the future. 

trip_majority <- trip_majority[-which(rowSums(trip_majority)==0),]

# convert to simple triplet matrix
triplet <- as.simple_triplet_matrix(trip_majority)

saveRDS(triplet, file="/Users/efuller/1/CNH/Analysis/Metiers/results/2014-09-06/trip_mat.RDS")
