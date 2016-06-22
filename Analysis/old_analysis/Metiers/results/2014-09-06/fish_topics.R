# trying to coerce catch data into format for topic models
library(devtools)
library(topicmodels)
library(tm)
library(Rmpfr)
library(plyr)
library(XML)
library(stringi)
library(dplyr)
library(reshape2)
library(slam)

# load fish tickets
ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors=F)

# let's subset to Astoria tickets from 2012
astoria_trips <- subset(ftl, pcid=="AST" & year=="2012", select=c("spid", "ftid","landed_wt"))

# select relevant data, construct trip matrix
melt_astoria <- melt(astoria_trips, variables = c("spid","ftid"), measure.vars = "landed_wt")
cast_astoria <- dcast(melt_astoria, ftid ~ spid, fun.aggregate = sum)
rownames(cast_astoria) <- cast_astoria$ftid
cast_astoria$ftid <-NULL

trip_mat <- as.matrix(cast_astoria)

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

# Fit models and find an optimal number of topics as suggested by Ben Marmick --
# http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

burnin <- 10 #1000
iter <- 10 #1000
keep <- 5 #50
ks <- 2 #seq(2, 250, by = 1)

model_short <- lapply(ks, function(k) LDA(triplet, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep, verbose = 5)))
