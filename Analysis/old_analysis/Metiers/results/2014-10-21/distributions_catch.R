library(reshape2); library(plyr); library(dplyr); library(scales)
# examining distribution of species in catch. 
  ftl <- read.csv("/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", stringsAsFactors = F)

# first make trip table (col = sp, row = trip)

  # because ftid is not unique by year, compile ftid and year together
  ftl$trip_id <- paste0(ftl$ftid, ftl$year)
  melt_ftl <- melt(ftl, id.vars = c("veid","trip_id","spid","tdate","grid"), measure.vars = "landed_wt")
  # not fast transposing
  cast_ftl <- dcast(melt_ftl, trip_id ~ spid, fun.aggregate = sum)

# make boxplots, but exclude any 0 catches, so those are NA
cast_ftl[cast_ftl==0] <- NA 

# reorder, so from highest to lowest median catch
bymedian <- with(melt_ftl, reorder(spid, -value, median))

boxplot(value ~ bymedian, data = melt_ftl, cex = .15, pch =19, las=2,col=alpha("black", .25))

# what about the number of trips these species show up in. Assuming that there is only one species entry for each trip

num_trips <- sort(table(melt_ftl$spid), decreasing=T)
# conditions for dropping should be that it's rarely caught, and of the times that it is caught it's always in small amounts. Avoids things like yellowfin tuna, who is sometimes caught in large volumes and for which there is a market.
medians <- rep(NA, length(num_trips))
sd <- rep(NA, length(num_trips))
for(i in 1:length(num_trips)){
  medians[i] <- median(melt_ftl$value[which(melt_ftl$spid == names(num_trips[i]))])
  sd[i] <- sd(melt_ftl$value[which(melt_ftl$spid==names(num_trips[i]))])
}

spdf <- data.frame(spid = names(num_trips), num_trips = num_trips, median_catch = medians, sd = sd)
little_spdf  <- subset(spdf, num_trips < 100 & median_catch < 100)
with(little_spdf, plot(num_trips, median_catch))
with(little_spdf, arrows(x0 =  num_trips, y0 = median_catch - sd,x1 = num_trips, y1=median_catch + sd, length = 0))
text(little_spdf$num_trips, little_spdf$median_catch, little_spdf$spid, cex = .85)
# save this list for manuscript

saveRDS(little_spdf, file="/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-01/little_spdf.RDS" )

# remove species from trip table

filter_trips <- cast_ftl[,!(names(cast_ftl) %in% little_spdf$spid)]

## remove trips which have zero catch

filter_trips <- filter_trips[-which(rowSums(filter_trips[,-1], na.rm=T)==0),]

# make new ftl dataset that only has correct trips

filtered_ftl <- subset(ftl, trip_id %in% filter_trips$trip_id)
saveRDS(filtered_ftl, "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-01/filtered_ftl.RDS")

filtered_ftl <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-01/filtered_ftl.RDS")

# using half 2010 data, train KNN to classify trips

# try for one gear type
tls10 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/metier_lists/TLS2010.csv", stringsAsFactors=F)

tls12 <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-10-08/metier_lists/TLS2012.csv", stringsAsFactors = F)

tls10$X <- NULL
tls10$year <- 2010

tls12$X <- NULL
tls12$year <- 2012

# merge into ftl

ftl_sub <- merge(filtered_ftl, tls10, by = c("ftid","year"))
ftl_sub12 <- merge(filtered_ftl, tls12, by = c("ftid","year"))

# should drop any  metier that has fewer than 3 vessels that participate

num_vess <- ddply(ftl_sub, .(metier), summarize, ves = length(unique(veid)))
num_vess12 <- ddply(ftl_sub12, .(metier), summarize, ves = length(unique(veid)))


# got this function from 
# http://stackoverflow.com/questions/20041239/how-to-randomly-split-a-data-frame-into-three-smaller-ones-with-given-numbers-
#' Splits data.frame into arbitrary number of groups
#' 
#' @param dat The data.frame to split into groups
#' @param props Numeric vector. What proportion of the data should
#'              go in each group?
#' @param which.adjust Numeric. Which group size should we 'fudge' to
#'              make sure that we sample enough (or not too much)

split_data <- function(dat, props = c(.8, .15, .05), which.adjust = 1){
  
  # Make sure proportions are positive
  # and the adjustment group isn't larger than the number
  # of groups specified
  stopifnot(all(props >= 0), which.adjust <= length(props))
  
  # could check to see if the sum is 1
  # but this is easier
  props <- props/sum(props)
  n <- nrow(dat)
  # How large should each group be?
  ns <- round(n * props)
  # The previous step might give something that
  # gives sum(ns) > n so let's force the group
  # specified in which.adjust to be a value that
  # makes it so that sum(ns) = n
  ns[which.adjust] <- n - sum(ns[-which.adjust])
  
  ids <- rep(1:length(props), ns)
  # Shuffle ids so that the groups are randomized
  which.group <- sample(ids)
  split(dat, which.group)
}

melt_ftl <- select(ftl_sub12, trip_id, spid, landed_wt, metier)
melt_ftl <- melt(melt_ftl, id.vars = c("trip_id","spid","metier"), measure.vars = "landed_wt")
cast_ftl <- dcast(melt_ftl, trip_id ~ spid, fun.aggregate = sum)
rownames(cast_ftl) <- cast_ftl$trip_id
cast_ftl$trip_id <- NULL

# need to make sure all metiers are represented in test dataset. 

df <- split_data(cast_ftl, props=c(.6,.4))

ref_metier <- select(melt_ftl, trip_id, metier)
ref_metier <- unique(ref_metier)

train <- df[[1]]
cl <- melt_ftl$metier[which(ref_metier$trip_id %in% rownames(train))]

test <- df[[2]]

library(class)

k_try <- knn(train, test, cl, k = 50, prob=TRUE )

val_test <- data.frame(trip_id = rownames(test), knn = k_try)
val_test <- merge(val_test, ref_metier, by = "trip_id")

val_test$match <- val_test$knn == val_test$metier
table(val_test$match)/nrow(val_test)

## question is which metiers are messed up
table(val_test$match, val_test$metier)

# looks like a bunch of small ones. the major two (albc and salm) are largely fine. but unsure why these two are getting confused. might have to wait until i get cleaner data. But also bump up the number of nearest neighbors and we look good. Should generate an ROC curve though to justify.  

# look at probabilities 

probs <- attributes(k_try)[[3]]

val_test$probs <- probs

# knn does badly with small metiers... why. 

# problem ones 
problems <- subset(val_test, metier %in% c("TLS_3","TLS_4","TLS_5","TLS_6"))

# TLS_6 = rockfish
# TLS_5 = lingcod and yellowtail
# TLS_4 = CHL and WBAS
# TLS_3 = CHL
# TLS_2 = albc
# TLS_1 = salmon
hist(problems$probs, breaks = 30)
