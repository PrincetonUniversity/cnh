# 2014-02-18
# Goal: To build a dataset out of observer data that matches Jonas' data set, and to use his code to run a similar analysis. This will both help me better undersatnd the methods and prepare for when we have the logbook data

# Building a dataset for analysis
## Jonas has two datasets he uses, a catch dataset in which (see below)

# Trip ID | Species 1 | Species 2 | Species 3 | ... | Species N
# -------------------------------------------------------------
# Species measures can be in proportions of weight or value (if I have it)
# A cluster analysis is performed on the species to get a cluster ID for each trip, which is then fed into the next analysis
#
# Second dataset is of the effort data
# Trip ID | Trip length | Number of sub-squares visited* | Gear Type | Mesh Size | Lat | Lon | catch cluster ID |
# ---------------------------------------------------------------------------------------------------------------
# * this is some attempt to categorize spatial patterns. Big statistical management areas split into smaller units
#   so this is the number of sub-units a vessel goes to in a single trip (or something). I think there's room for improvement here, especially for for VMS data. 
# The lat/lon is actually the statistical management area where the catch took place, and also there's a seperate measure of the number of sub squares in which a fishermen fished. I think there is room for improvement here for the observer data since we know exactly where they were. But with the logbooks we won't.. Although, again with VMS, we will... Think I could try the same clustering approach for space and see how that lines up with bathymetry and/or habitat type. Berdahl made a good point that clustering only on space won't do a great job with long narrow shapes. For now, perhaps will use statistical management area measures. 

# building catch dataset

# structure of dataset is that each row is an entry about a given haul. so need to aggregate species information for all trips together. 
# according to NWFSC Observer page: http://www.nwfsc.noaa.gov/research/divisions/fram/observation/data_collection/index.cfm
# The weight of retained catch by species or species group is recorded
# but also adjust the catch so that it matches what is reported on the fish ticket data. 
# for catch the options include
# - CATCH_CATEGORY_CODE: WCGOP assigned catch categories
# - COMMON_NAME: common name as defined by the WCGOP databse
# - RET: ftadj weight (mt) of retained catch (ftadj - DIS = RET)
# - spc.name: Common name of species, provided by an external reference file designed to standardize either the WCGOP database or the PacFIN database data source (different than 'species' data field below)
# - COMMON_NAME: Common name of species as defined by the WCGOP database (not completed for src=FT) (has slightly more than spc.name - thinks like Kelp/Rocks/Wood/Mud or Sponge instead of NA)



# other things
# - cmplx: Variable to assign groundfish management complexes or if they're managed outside a complex (could be fun to see how fishing strategies fish across these complexes or just within them)
# - D_PORT: port from which the vessel departed (would be fun to see if particular clusters of fishing strategy are associated with particular ports)
# - D_PORT_GROUP: WCGOP port grouping of vessel departure (related to above)

# method: use spc.name and RET and aggregate by TRIPID
require(dplyr)
require(reshape2)

obs <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Obs/Data/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")
obs_df <- tbl_df(obs)
obs_df <- select(obs_df,TRIPID,spc.name,RET)

# calculate trip totals
totals <- obs_df %.%
  group_by(TRIPID) %.%
  summarise(total = sum(RET)) %.%
  arrange(TRIPID)

# calculate by species total catch for each trip
catch <- obs_df %.%
  group_by(TRIPID,spc.name) %.%
  summarise(catch = sum(RET)) %.%
  filter(catch > 0) %.%
  arrange(TRIPID)

  # remove NAs as species name
  catch <- catch[-which(is.na(catch$spc.name)),]

# for each entry for species caught, need to find total proportion of catch (divide by catchRET$total)
  total_catch <- dcast(catch, TRIPID ~ spc.name, fill = 0, drop=TRUE)
  # make names better
  names(total_catch) <- gsub(" ","_",names(total_catch))

# combine totals and species composition 
  cluster <- merge(total_catch, totals, by="TRIPID")
# check, do the rows sum to give total?
  length(which(rowSums(cluster[,2:170])/cluster$total!=1))
  # there are about 8% of rows that don't match, why not? dunno. I think this is a precision issue, manually examining looks like lots are the same.

cluster_df <- tbl_df(cluster)

# doing cluster analysis using code from Jonas: catch_profiles_lb_data_95-09.R
library(cluster)

# Functions
  n.no0 <- function(x) length(x[x > 0])
  reallength <- function(x) length(x[x==0])
  mean.no0 <- function(x) mean(x[x > 0.1])

# remove species that occurred < 20 times
  
  # construct table
  freq <- apply(cluster_df[,2:171], 2, function(x) length(which(x > 0)))
  # remove those species that are in fewer than 20 trips
  cluster_sub <- cluster_df[,-which(names(cluster_df) %in% names(freq)[which(freq<20)])]
  # remove rows that have no catch from any target species
  cluster_sub <- cluster_sub[-which(rowSums(cluster_sub[2:(ncol(cluster_sub)-1)])==0),]

# PCA analysis of catch profiles
  # take only species data
  pca.dat1 <- cluster_sub[2:(ncol(cluster_sub)-1)]

  # Log quantities as PCA indata (Jonas found results to be quite similar, should check)
  pca.dat2 <- pca.dat1+0.1
  pca.dat2 <- log(pca.dat2) 

  # Species proportion as PCA indata
  sp.prop <- pca.dat1/(rowSums(pca.dat1))

# PCA analysis (should think about doing it on amounts, not proportions)

pca.prop <- prcomp(pca.dat1, scale = TRUE) #(if you use raw data the common and extremely rare species will influence the loading, but if you scale it before you put it into the PCA then all the species will be much more equal in influence over loading, other option will be scale = TRUE, will normalize)
# plot(pca.prop) shows how each component explains the variance
# biplot(pca.prop)
pca.scores <- pca.prop$x
# see how many PCs needed to explain 95% of variance
summary(pca.prop)    # 96

clusts.prop = vector("list",length=96)   # 96 is the maximum number of clusters (between 2:96 in the case). clara makes the best solution given that number of clusters. We don't know how many different clusters so loop through them. Look at how many species constitute 95% of the catch. 
data <- pca.scores[,1:96] # that was the 90-95% variance explained by PCA. So in my case I'd use a few more. 
for (i in 1:96) {clusts.prop[[i]] <- clara(data, (i+1), metric = "euclidean", stand = TRUE, samples = 100, sampsize = 300); print(i) }
# samples and sampsize are variables that determine how the full dataset if sampled to make clusters. 100, 300 are more then most experts recommend. Should look at Kauffman and ... (check Jonas' paper). 

# Objective function and ASW
objectives <- vector(length = 96)
for (i in 1:96) { objectives[i] <- clusts.prop[[i]]$objective }
asw <- vector(length = 96)
for (i in 1:96) { asw[i] <- clusts.prop[[i]]$silinfo$avg.width  }

# Finding a jump in the objective function
# x = 8 (representing 9 clusters) is a peak in the explanation level
# For now, use 9 clusters
quartz(width = 6, height = 6); plot(3:97, objectives[2:96]-objectives[1:95], type = "o", pch = 19, ylab = "jump in Objective function", xlab = "Number of clusters", main = "Reduction in Unexplained variation by adding one cluster"); abline(h = 0, lty = 2)
abline(v = seq(3,99,2), col = "lightgrey", lty = 2)

# Objective function
quartz(width = 5, height = 5); plot(2:97, objectives, type = "o", xlab = "Number of clusters", ylab = "Objective function")
abline(v = seq(2,100,2), col = "lightgrey", lty = 2)

# ASW
quartz(width = 5, height = 5); plot(2:97, asw, type = "o", xlab = "Number of clusters", ylab = "Average silhoutte width")
abline(v = seq(2,20,2), col = "lightgrey", lty = 2)
# this shows how well defined each cluster is, 0 is not clustered at all, 1 is completely clustered. 

# Cluster solution characteristics - 16 looked like a nice amount of clusters to keep
clusts.prop[[16]]$clusinfo
clusts.prop[[16]]$silinfo


# Choose one clustering solution
clust <- clusts.prop[[16]]
CP <- clust$clustering  

# Which species characterize every cluster? Mean quantities and mean Proportions
species.quant <- t(aggregate(pca.dat2, list(CP), mean))[2:ncol(pca.dat2)+1,]
species.quant2 <- t(aggregate(pca.dat1, list(CP), mean))[2:ncol(pca.dat1)+1,]
species.prop <- t(aggregate(sp.prop, list(CP), mean))[2:ncol(pca.dat2)+1,]
# what are columns here?


# plots
# what is the freq distribution of different species

for(i in 1:ncol(sp.prop)) {hist(sp.prop[,i], breaks = seq(0,1,0.05), main = names(sp.prop[i]))}

# 8 is the top 8 species. 

# number of trips for each catch profile

dump <- vector("list", 16)
par(mfrow = c(5,4), mar = c(7,1,2,1), oma = c(1,4,2,0))
for (i in 1:max(CP)) {
  plotdata <- pca.dat1[CP == i,]
  occurence <- apply(plotdata, 2, n.no0)
  # dump[[i]] <- (sort(occurence, decreasing = TRUE)/nrow(plotdata))[1:8] # proportion of trips 
  dump[[i]] <- (sort(occurence, decreasing = TRUE))[1:20] # number of trips
 # barplot((sort(occurence, decreasing = TRUE)/nrow(plotdata))[1:8], cex.names = 0.8, las = 3) # proportion of trips
  barplot((sort(occurence, decreasing = TRUE))[1:20], cex.names = 0.8, las = 3) # number of trips
  legend("top", paste("Catch profile =", i), bty = "n") }
mtext(side = 3, "Frequency of Occurence per species and cluster (16 cluster version, Sp. prop. PCA)", outer = TRUE)
mtext(side = 2, "Total Caught (lbs)", outer = TRUE, padj = -1)

# quantities per catch profile - use merged_catch
foo <- subset(cluster_sub, select=c(TRIPID, profile))

merged_catch <- merge(catch, foo, by ="TRIPID")

# for TRIPIDs grouped by profile 

mean_catch <- ddply(merged_catch, .(profile, spc.name), function(df) mean(df$catch))
names(mean_catch)[3] = "meanCatch"
# take only top 20 species for each profile
  # take each profile, find top 10 species, discard rest

top_catch <- ddply(mean_catch, .(profile), subset, rank(meanCatch)>=10 ) # this is not working quite right. 

top_catch <- transform(top_catch, spc.name = reorder(spc.name, - meanCatch)) # make it so the spc.name factors are ordered by meanCatch values
top_catch <- top_catch[order(top_catch$profile,top_catch$meanCatch),]


ggplot(top_catch, aes(y = meanCatch, x = spc.name, fill=factor(spc.name))) + geom_bar(stat="identity") + facet_wrap(~ profile, ncol=4,scales="free")

# find mean quantity of species caught


# with the 16 cluster situation, seems like most of the big trips are catch profiles 1-3, although 8-13 are small and look targetted. 14-17 look like mistakes, forced to be recorded by obesrvers (they're of bycatch quota species primarily and really small). 

# ideas from Jonas: try log quantity, it's better for count data. 



##################### MAPPING ATTEMPS ###################################
# use cluster_sub to match to lat/long of TRIP ID to map catch profiles

# make obs just tripID and lat/lon
loc_obs <- subset(obs,select=c(TRIPID,SET_LAT,SET_LONG))
foo <- subset(cluster_sub, select=c(TRIPID, profile))

bar <- merge(foo, loc_obs, by = "TRIPID")

require(RColorBrewer)

buffer=0

colorz <- colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(bar$profile)))
plot(bar$SET_LONG, bar$SET_LAT, col=colorz[bar$profile],pch=19,cex=0.25,xlim=range(bar$SET_LONG)+buffer, ylim=range(bar$SET_LAT)+buffer,xlab="Longitude",ylab="Latitude",bty="n")

xyplot(SET_LAT ~ SET_LONG | factor(profile), data=bar, col=colorz pch=19, cex=0.25, panel.)

require(maps)
require(mapdata)
require(ggmap)
map("worldHires","USA", col="gray", fill=TRUE, add=TRUE, border=FALSE)


bestcoast <- map_data("usa")

ggplot(bar, aes(x=SET_LONG, y = SET_LAT)) + geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group)) + geom_point(aes(colour=factor(profile))) + facet_wrap(~ profile, ncol=4) + coord_map(xlim=range(bar$SET_LONG), ylim=range(bar$SET_LAT)) + theme_few()

# from histogram of species looks like we have 3 different categories. 
# big fisheries: profiles 1-3, sablefish multispeices, hake, and pink shtrimp. trips are in the 100s
# small fisheries: profiles 4-7, lingcod, unidentified shrimp, mixed rockfish, and tanner crab/sablefish. trips are in the 10s
# smaller: profiles 8-13, White Croaker/Pacific Sanddab, Petrale Sole; Mixed rockfish; Arrowtooth flounder, lingcod, hake; Rockfish/DTS; another crab/sablefish. Catches are in the 5s of trips
# tiny: I think these are bycatch quota hauls, profiles 14-17. Have Bocaccio, canary, etc. But not totally sure. These are ~ 1 trip.

# what about quantities of catch?

# also looks like there are 3 main profiles here. But also, how does by haul change?

subset different fisheries to plot seperately

big3 <- subset(bar, profile==1 | profile==2 | profile==3)
rest <- bar[-which(bar$profile==1 | bar$profile ==2 | bar$profile ==3),]
small <- subset(bar, profile==4 | profile==5 | profile==6 | profile==7)
smaller <- subset(bar, profile==8 | profile==9 | profile==10 | profile==11 | profile == 12 | profile==13)
smallest <- subset(bar, profile == 14 | profile == 15 | profile == 16 | profile == 17)

ggplot(big3, aes(x=SET_LONG, y = SET_LAT)) + geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group)) + geom_point(aes(colour=factor(profile))) + facet_wrap(~ profile, ncol=3) + coord_map(xlim=range(bar$SET_LONG), ylim=range(bar$SET_LAT)) + theme_few()

colorz <- colorRampPalette(brewer.pal(8,"Dark2"))(length(unique(rest$profile)))
plot(rest$SET_LONG, rest$SET_LAT, col=colorz[bar$profile],pch=19,cex=1,xlim=range(rest$SET_LONG)+buffer, ylim=range(rest$SET_LAT)+buffer,xlab="Longitude",ylab="Latitude",bty="n")
legend("bottomleft",legend=unique(rest$profile),col=colorz,pch=19)

# looking at how frequently hauls are part of one trip versus another find that the top 1 (sablefish) way swamp anything else. But pacific hake is up there. And remember this is frequency of hauls. Although it also happens that the amount of sablefish being pulled in on these trips is bigger than hake. 
hist(bar$profile)

# when remove the top 3 can ses the variability 
hist(rest$profile)

# although this leaves out profile 3 (pink shrimp). 

# but regardless, different than the pattern just based 