# making foodwebs
library(dplyr); library(igraph); library(Hmisc)
# nodes are strategies-groups and species. edges are the weight in pounds that strategy group harvested

s09 <- strategies[[1]]$yr_trips
strat <- select(s09, strategy, veid)

f09 <- subset(metiers, year == 2009 & veid %in% s09$veid)
f09 <- merge(f09, strat, by="veid", all.x=TRUE)

melt_trips <- melt(f09, id.vars = c("veid","ftid","spid","tdate","grid","metier","strategy"), measure.vars = "landed_wt")
cast_trips <- dcast(melt_trips, strategy ~ spid, fun.aggregate = sum)
row.names(cast_trips) <- cast_trips$strategy
cast_trips$strategy <- NULL
cast_trips <- as.matrix(cast_trips)
foodweb <- graph.incidence(cast_trips, weighted = TRUE)
V(foodweb)$degree <- degree(foodweb)
par(mai=rep(0,4))
plot(foodweb, edge.width = log(E(foodweb)$weight)/20, layout= layout.bipartite, vertex.size = 1, vertex.label.cex = V(foodweb)$degree/(max(V(foodweb)$degree)/2), vertex.color = "white", vertex.label = V(foodweb)$name)

# looking at distributions of metiers within a strategy
par(mai=c(2,1,0.3,.1))
barplot(sort(table(subset(f09, strategy==2)$metier),decreasing=T),bor=F,las=2)

# another idea is to color big foodweb network by metiers, one at a time. so all species-strategies connections that were mediated by HKL_5_2009 would be colored, for example. 

# requires me to find strategies which contain HKL_5_2009. And then which species HKL_15_2009 has in it. And then coloring the combinations of connections that go between those strategies and those species. 

# so which strategies contain HKL_5_2009
unique(subset(f09, metier=="HKL_5_2009")$strategy)

strats <- as.numeric(V(foodweb)[unique(subset(f09, metier=="HKL_5_2009")$strategy)])

# which species are in strategy 2 and 10
sp <- unique(subset(f09, strategy %in% c(2,10) & metier == "HKL_5_2009")$spid)

E(foodweb)$color <- alpha("grey",.15)
E(foodweb)[strats %--% sp]$color <- "dodgerblue"

par(mai=rep(0,4))
plot(foodweb, edge.width = log(E(foodweb)$weight)/20, layout= layout.bipartite, vertex.size = 1, vertex.label.cex = V(foodweb)$degree/(max(V(foodweb)$degree)/2), vertex.color = "white", vertex.label = V(foodweb)$name, axes=TRUE)

# what about getting trophic level from fishbase
library(rfishbase)
data(fishbase)
# get common names from pacfin
spid <- read.csv("/Users/efuller/1/CNH/Analysis/Metiers/data/spid.csv",stringsAsFactors=F)
# subset to just species
spid <- subset(spid, X==1)

# find species
science_names <- data.frame(spid = colnames(cast_trips))
just_science <- select(spid, SPID, science_name)
science_names <- merge(science_names, just_science, by.x = "spid", by.y = "SPID", all.x = TRUE, all.y = FALSE)

science_names$science_name <- tolower(science_names$science_name)
# but capitalize first word of each string
science_names$science_name <- capitalize(science_names$science_name)

myfish <- findSpecies(science_names$science_name[2])
as.numeric(getTrophicLevel(fish.data[myfish]))

rockfish <- which_fish("Sebastes", using = "Genus")
one_rock <- rep(NA, length(rockfish))
which_rockfish <- which(rockfish == TRUE)
trophs <- rep(NA, length(which_rockfish))
for(i in 1:length(which_rockfish)){
  catch <- tryCatch(getTrophicLevel(fish.data[which_rockfish[i]]), 
                    error=function(e) e, warning=function(w) w)
  if(is(catch, "simpleError")) next()
  trophs[i] <- getTrophicLevel(fish.data[which_rockfish[i]])
  cat(i," ")
}

# find science names that are not Sebastes and not NA
other_names <- subset(science_names, !is.na(science_name) & science_name!="Sebastes spp.")

other_trophic <- data.frame(science_name = other_names, trophic_level = rep(NA, length(other_names)))

# in loop, first need to find the species, if not NA, then can look for trophic level with conditional warning statement


for(i in 1:nrow(other_trophic)){
  # check if species exists
  sp <- findSpecies(other_trophic$science_name[i])
  # if it does, check for trophic level
  if(length(which(sp==TRUE))==1){
    catch <- tryCatch(getTrophicLevel(fish.data[sp]), 
                      error=function(e) e, warning=function(w) w)
    # if returns an error, step to next round
    if(is(catch, "simpleError")) next()
    # but otherwise 
    other_trophic$trophic_level[i] <- catch
    cat(i," ")
  }
}

l <- as.data.frame(layout.bipartite(foodweb))
l$name <- as.character(V(foodweb)$name)

to_merge <- select(other_trophic, spid, trophic_level)
# merge the trophic level in
  l_new <- merge(l,to_merge,by.x = "name", by.y="spid", all.x = TRUE )

# merge in trophic level for rockfish species
  rock_trophic <- mean(trophs, na.rm=T)
  science_names$trophic <- ifelse(science_names$science_name=="Sebastes spp.", rock_trophic, NA)
  to_merge_rocks <- subset(science_names, science_name == "Sebastes spp.", select = -science_name)

  colnames(to_merge_rocks) <- c("name","trophic_level")
  l_new$trophic_level[which(l_new$name %in% to_merge_rocks$name)] <- rock_trophic

# combine the trophic levels

# adjust coordinates based on trophic level. 
l_new$V2 <- ifelse(!is.na(l_new$trophic_level), l_new$V2 + .1*l_new$trophic_level, l_new$V2)

l_layout <- as.matrix(l_new[,2:3])
plot(foodweb, layout=l_layout, vertex.label.cex = .4, vertex.size = 5, edge.weight = .5)

# existing problems: the nominal versus non-nominal (arrowtooth for example). Foodweb nodes should be combined, as should the trophic level. 
