# this is based on a 23 cluster solution to catch profiles. then figuring out which are 'risky' catch profiles based on catch abundance of bycatch species. then looking at their distribution over time. 

require(cluster)
require(ggplot2)
require(ddply)
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
cluster_df <- tbl_df(cluster)

# construct table
freq <- apply(cluster_df[,2:171], 2, function(x) length(which(x > 0)))
# remove those species that are in fewer than 20 trips
cluster_sub <- cluster_df[,-which(names(cluster_df) %in% names(freq)[which(freq<20)])]
# remove rows that have no catch from any target species
cluster_sub <- cluster_sub[-which(rowSums(cluster_sub[2:(ncol(cluster_sub)-1)])==0),]
# we've lost 2 trips, and 59 species

pca.dat1 <- cluster_sub[2:(ncol(cluster_sub)-1)]
# Log quantities as PCA indata
pca.dat2 <- pca.dat1+0.1
pca.dat2 <- log(pca.dat2) 
pca.dat2 <- prcomp(pca.dat2, scale = TRUE) 
pca.scoresdat2 <- pca.dat2$x
data.dat2 <- pca.scoresdat2[,1:84] # that was the 90-95% variance explained by PCA.

# cluster on 23
clusts.dat2 <- clara(data.dat2, 24, metric = "euclidean", stand = TRUE, samples = 100, sampsize = 300)

clust <- clusts.dat2
CP <- clust$clustering  

cluster_sub$profile = CP
subdf <- select(cluster_sub,TRIPID, profile)

catchProfile <- merge(catch, subdf, by = "TRIPID")

# looking at how different catch profiles vary over the year, between 'risky' catch profiles. Also looking at distribution of generalist versus specialist catch profiles. 

# first identify 'risky' catch profiles. These are any profiles that include bocaccio rockfish, canary rockfish, cowcod, darkblotched rockfish, pacific ocean perch, petrale sole, widow rockfish, yelloweye rockfish. This comes from this NOAA report (http://www.nmfs.noaa.gov/stories/2012/07/docs/catch_sharesyear1_report.pdf). 


  # for each catch profile, plot proportion of these species summed as proportion of total species abundance and mean trip abundance. 

  # Mean trip abundance: take mean amount of catch by species by trip - all
  catchProfile$risky <- rep(0, nrow(catchProfile))
  catchProfile$spc.name = as.character(catchProfile$spc.name)
  DR <- which(catchProfile$spc.name=="Darkblotched Rockfish")
  PS <-  which(catchProfile$spc.name=="Petrale Sole")
  POP <-  which(catchProfile$spc.name=="Pacific Ocean Perch")
  WR <-  which(catchProfile$spc.name=="Widow Rockfish")
  BR <-  which(catchProfile$spc.name=="Bocaccio Rockfish")
  CR <- which(catchProfile$spc.name=="Canary Rockfish")
  CC <- which(catchProfile$spc.name=="Cowcod Rockfish")
  YR <- which(catchProfile$spc.name=="Yelloweye Rockfish")
  catchProfile$risky[DR]=1
  catchProfile$risky[PS]=1
  catchProfile$risky[POP]=1
  catchProfile$risky[WR]=1
  catchProfile$risky[BR]=1
  catchProfile$risky[CR]=1
  catchProfile$risky[CC]=1
  catchProfile$risky[YR]=1
  require(plyr)
  bdown_risk <- ddply(catchProfile, .(profile, risky), summarize, meancaught = mean(catch))
  # catch profiles 9,11 has no risky species, need to add 0 in
  add <- c(9, 1, 0)
  add2 <- c(11, 1, 0)
  bdown_risk <- rbind(bdown_risk, add, add2)
  # plot by profile, the mean amount of each species caught  
  ggplot(bdown_risk, aes(x = factor(profile), y = meancaught, fill = factor(risky))) + geom_bar(stat="identity")

  # what's the distribution of proportions of catch?
  prop_risk <- ddply(bdown_risk, .(profile), transform, prop = meancaught/sum(meancaught))
  sub_risk <- subset(prop_risk, risky==1)
  # order bars from most to least bycatch
  prop_risk$profile <- reorder(prop_risk$profile, rep(sub_risk$prop, each = 2))
    ggplot(sub_risk, aes(x=prop)) + geom_histogram(binwidth=0.05)
    ggplot(prop_risk, aes(x = factor(profile), y = prop, fill = factor(risky))) + geom_bar(stat="identity") + geom_abline(intercept=.9, slope=0, color="white", size = 2)

# based on this, anything that is has less than 90% bycatch is called risky
  # which profiles?
  yikes_profiles <- sub_risk$profile[sub_risk$prop>.1]

  catchProfile$yikes = rep(0, nrow(catchProfile))
  catchProfile$yikes[which(catchProfile$profile %in% yikes_profiles)] = 1
  subdf <- select(catchProfile, TRIPID, yikes)

  overTime <- select(obs, TRIPID, SET_MONTH, SET_YEAR, SET_LAT, SET_LONG)
  overTime <- merge(overTime, subdf, by = "TRIPID")
  overTime <- overTime[!duplicated(overTime),] 
  # there's a 2001 year in here, remove it
  overTime <- overTime[-which(overTime$SET_YEAR==2001),]

  ggplot(overTime, aes(x = factor(SET_MONTH), fill = factor(yikes))) + geom_bar() +facet_wrap(~SET_YEAR)

  # want month, year, and the number and proportion of risky trips
  numRisk <- ddply(overTime, .(SET_MONTH, SET_YEAR), transform, nrow(TRIPID))
  plot(overTime, )

# see the species composition of each cluster
  bdown <- ddply(catchProfile, .(profile, spc.name), summarize, meancaught = mean(catch))
  ranked <- ddply(bdown, .(profile), transform, rank(meancaught))
  small <- ddply(ranked, .(profile),  function(df) df[order(df$meancaught,decreasing=TRUE)[1:5],])

# plot by profile, the mean amount of each species caught
  ggplot(small, aes(x = spc.name, y = meancaught, fill=factor(spc.name))) + geom_bar(stat="identity") + facet_wrap(~profile, scale="free") + theme(axis.text.x = element_text(angle = 0))+ theme(legend.position="bottom") + guides(fill=guide_legend(nrow=3))
                              
# more generally, should look at distribution of these target species in different catch profiles. 

# also identify generalist and specialist catch profiles. specialist catch profiles are ones dominated by a single species. look to see how the distribution of other species varies. 
  
  # look at proportion of total catch made up by species (pie charts?) and then by mean catch.
  propdown <- ddply(catchProfile, .(profile), transform, prop = catch/sum(catch))
  propdown_synth <- ddply(propdown, .(profile, spc.name), summarize, mean_prop = mean(prop))
  ggplot(propdown, aes(x = factor(profile), y = prop, fill = factor(spc.name))) + geom_bar(stat="bin") + theme(legend.position="bottom") + guides(fill=guide_legend(nrow=3))
# I think this will need to be re-done with estimated value of each species. For example: sablefish are caught in low quantities, but I suspect targeted often. So won't show up in total volume, but will be targetted in a single-species type way nonetheless. 