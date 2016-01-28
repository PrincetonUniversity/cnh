# what constitutes a small boat? (examining length data)
library(dplyr)
# length distribution - what are small boats?
ftl_len <- readRDS("/Users/efuller/1/CNH/processedData/catch/5_add_length/ftl_len.RDS")

# see distribution of boats that ever landed
  specs <- unique(ftl_len[,c("drvid","year","len","hp")])
  hist(specs$len,breaks=50,col="grey",bor="white", freq = F)
  lines(density(specs$len, na.rm = T), col = "indianred",lwd=3 )
  
  # how many boat-years missing length data?
  length(which(is.na(specs$len)))/nrow(specs) # very few. nice. 
  
  # could I reduce that by applying length data across years?
  #find which boats missing data
  miss_values <- unique(specs$drvid[which(is.na(specs$len))])
  
  #RETURN TO THIS#

# see length distribution of landings weighted by biomass
  met_len <- ftl_len %>%
    group_by(trip_id) %>%
    summarize(metier = unique(metier), cluster = unique(cluster), dahl_sector = unique(dahl_sector), len = unique(len), pcid = unique(pcid), year = unique(year), agid = unique(agid), lbs = sum(landed_wt))
    
    unique(ftl_len[,c("trip_id","metier","cluster","dahl_sector","len","pcid","year","agid")])
  
  # see length distribution weighted by number of trips
  hist(met_len$len, breaks = 50, col = "lightslategrey",bor="white", add = F, freq = F)
  lines(density(ftl_len$len, na.rm = T), col = "black",lwd = 2 )
  
  plot(density(met_len$len, na.rm = T), col = "lightslategrey",lwd=3 )
  lines(density(specs$len, na.rm = T), col = "grey",lwd=3 )
  
  # _slightly_ shifted to the left for smaller boats
  
  # what percentage of trips don't have length data
  length(which(is.na(met_len$len)))/nrow(ftl_len) # about 0.15%, which is 14,419 trips
  
  # are they concentrated in any particular metier?
  trip_miss <- table(is.na(met_len$len),met_len$metier)[2,]
  # maybe, let's look in proportion to the amount of time each metier is landed
  all_mets <- table(met_len$metier)
  relative_miss_metier <- trip_miss/all_mets
  relative_miss_metier[relative_miss_metier > 0 ]
  
  # need metier names
  met_names <- read.csv("/Users/efuller/1/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp_gn.csv", stringsAsFactors = FALSE)
  
  met_miss <- data.frame(metier = names(all_mets), n.trips_all = as.vector(all_mets), p.trips_miss = as.vector(trip_miss/all_mets))
  
  met_miss$metier <- as.character(met_miss$metier)
  
  met_miss <- merge(met_miss, met_names, by.x = "metier",by.y = "Metier",all.x = TRUE, all.y = FALSE)
  
  # let's plot
  plot(met_miss$n.trips_all, met_miss$p.trips_miss, pch = 3, cex = .25, xlab= "# trips landed in a metier",ylab = "% of trips without length data",bty="n")
  text(met_miss$n.trips_all, met_miss$p.trips_miss, paste(met_miss$Major_species, met_miss$Major_gear, sep = "\n"), cex = .25, pos = 4)
  arrows(x0 = 40000, y0 = .6, x1 = 20000, y1 = .25, length = 0, angle = 45)
  arrows(x0 = 40000, y0 = .6, x1 = 20000, y1 = 1, length = 0, angle = 45)
  text(4000, .6, "These are (likely)\nbycatch fisheries\nor v. small", pos = 4,offset = 3.75,cex=.75)
  
  # of these, major ones are: TLS_1 is chinook salmon troll
  
  
  # what about biomass?
  # biomass in each metier
  met_lbs <- ftl_len %>%
    group_by(metier) %>%
    summarize(lbs = sum(landed_wt, na.rm = T))
  
  # biomass for trips where len is.na
  mis_lbs <- ftl_len %>%
    filter(is.na(len)) %>%
    group_by(metier) %>%
    summarize(m.lbs = sum(landed_wt, na.rm = T))
  
met_all <- merge(met_lbs, mis_lbs, by = "metier",all.x = TRUE, all.y = TRUE)
met_all$m.lbs[which(is.na(met_all$m.lbs))] <- 0
met_all$p.missed = met_all$m.lbs/met_all$lbs

plot(met_all$lbs, met_all$p.missed, pch = 3, cex = .25)
text(met_all$lbs, met_all$p.missed, met_all$metier, cex = .5, pos = 4)

# similar pattern, but the identity of "big fisheries" changes. No longer crab, but seiners and trawls. Still low though. 

# look at length distribution by catch biomass
hist(met_len$len,breaks=200,col='grey',bor='white') # sort of a break at 60

# but there are trawlers that are 36, 38 feet long.. don't think of trawlers as "small"

# plot distribution of lengths by metier

par(mfrow = c(1,2))
with(met_len, plot(density(len,na.rm=T), col = "black", lwd = 2, bty = "n", main = "distribution of vessel length", xlab = "length (ft)"))
abline(v=60, col = "grey", lty = 3)

pdf(file = "/Users/efuller/1/CNH/Presentations/2015_5_ESA_figs/length_distribution.pdf", width = 8, height = 6)
par(mai=c(.75,.75,0,0), cex.lab=1.1, cex.axis=.8)
with(subset(met_len, metier == "TWL_3"), plot(density(len,na.rm=T), col = "dodgerblue",xlim=c(0,150),bty = "n", xlab = "", ylim = c(0,.13), main="", ylab="", lwd=2), )
with(subset(met_len, metier == "TWL_1"), lines(density(len,na.rm=T), col = "orange",lwd=2))
with(subset(met_len, metier == "POT_1"), lines(density(len,na.rm=T), col = "red",lwd=2))
with(subset(met_len, metier == "TLS_1"), lines(density(len,na.rm=T), col = "pink",lwd=2))
with(subset(met_len, metier == "HKL_2"), lines(density(len,na.rm=T), col = "slategrey",lwd=2))
with(subset(met_len, metier == "TWS_1"), lines(density(len,na.rm=T), col = "magenta",lwd=2))
#with(subset(met_len, metier == "HKL_1"), lines(density(len,na.rm=T), col = "slateblue", lwd = 3))
#with(subset(met_len, metier == "POT_4"), lines(density(len,na.rm=T), col = "slateblue", lwd = 3))
mtext("Density",side = 2, line = 2)
mtext("Vessel length (feet)",side = 1, line = 2)
legend("topright",col = c("dodgerblue","orange","red","pink","slategrey"),lwd=3, legend = c("whiting midwater", "DTS trawl","Dungeness crab pot","chinook troll","black rockfish hook/line"), bty = 'n', cex = 1)

dev.off()

abline(v = 40, col = "grey",lty = 3)


with(subset(met_len, metier == "TLS_2"), lines(density(len,na.rm=T)))
with(subset(met_len, metier == "MSC_1"), lines(density(len,na.rm=T)))
with(subset(met_len, metier == "HKL_1"), lines(density(len,na.rm=T)))
with(subset(met_len, metier == "NET_2"), lines(density(len,na.rm=T)))
with(subset(met_len, metier == "HKL_3"), plot(density(len,na.rm=T)))
with(subset(met_len, metier == "HKL_4"), lines(density(len,na.rm=T)))

# if did use 60, get about 87% of trips with small boats
length(which(met_len$len<60))/nrow(met_len)

sum(met_len$lbs[which(met_len$len<60)])/sum(as.numeric(met_len$lbs)) # about about 34% of landings

# an aside, interested to see if size and diversity are inversely related in this dataset as well

# imagining I use 60 feet ...
length_split = 40

# amount of landings by small boats
  # number trips
  length(which(met_len$len <= length_split))

  # amount of biomass
  sum(met_len$lbs[which(met_len$len <= length_split)])

# what percentage of fleet
  # total trips
  length(which(met_len$len <= length_split))/nrow(met_len) # 87% trips
  
  # total biomass
  sum(met_len$lbs[which(met_len$len <= length_split)])/sum(as.numeric(met_len$lbs)) # 35%

# find top 95% of landings for small boats at each port. how many ports have POT_1 (crab) in them?
port_landings <- met_len[which(met_len$len <= length_split),] %>%
  group_by(pcid, metier) %>%
  summarize(total_lbs = sum(lbs)) %>%
  arrange(-total_lbs) %>%
  group_by(pcid) %>%
  mutate(cumulative_lbs = cumsum(total_lbs), all_lbs = sum(total_lbs), per.lbs = cumulative_lbs/all_lbs) %>%
  summarize(major_metier = min(per.lbs))


  
  

# how many boats are missing length data?


