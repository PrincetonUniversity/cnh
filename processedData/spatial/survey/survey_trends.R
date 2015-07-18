# survey data, trends in crab and sable fish
WORKING_DIRECTORY = "/Users/efuller/Downloads/Data_Vis_2015_07_07-2/"
wcannfish = read.csv(paste(WORKING_DIRECTORY, '/wcann_fish.csv', sep=''))
wcannhaul = read.csv(paste(WORKING_DIRECTORY, '/wcann_haul.csv', sep=''))
wcanninvert = read.csv(paste(WORKING_DIRECTORY, '/wcann_invert.csv', sep=''))
wcanncatch = rbind(wcannfish[,names(wcanninvert)], wcanninvert) # wcannfish has an extra column, so trim it out while combining with inverts
wcann = merge(wcannhaul, wcanncatch)

# merging is weird.. not sure i like the merge. I think there are duplicated rows, not sure why. Makes me worry about what it's merging on. Don't have time to troubleshoot, will assume it's fine
crab <- wcann[grep("magister",wcann$Species),]

crab <- crab[-which(duplicated(crab)),]
crab$dens <- crab$Haul.Weight..kg./crab$Area.Swept.by.the.Net..hectares.
crab$year <- as.numeric(format(as.Date(crab$Trawl.Date, "%m/%d/%y"), "%Y"))

library(beanplot)

beanplot(dens ~ year, data = crab, col = "indianred", ylab = year)

library(dplyr)

av_dens <- crab %>%
  group_by(year) %>%
  summarize(mean_dens = mean(dens), sd = sd(dens,na.rm=T))

plot(av_dens$year, av_dens$mean_dens, ylim = c(0,40))
arrows(x0 = av_dens$year, y0 = av_dens$mean_dens, x1 = av_dens$year, y1 =(av_dens$mean_dens + av_dens$sd))
arrows(x0 = av_dens$year, y0 = av_dens$mean_dens, x1 = av_dens$year, y1 =(av_dens$mean_dens - av_dens$sd))
library(beanplot)
beanplot(dens ~ year, data = crab, border = FALSE, col = "indianred", ylim = c(0.1, 1500))


sablefish <- wcann[grep("Anoplopoma fimbria", wcann$Species),]
sablefish <- sablefish[-which(duplicated(sablefish)),]
sablefish$dens <- sablefish$Haul.Weight..kg./sablefish$Area.Swept.by.the.Net..hectares.
sablefish$year <- as.numeric(format(as.Date(sablefish$Trawl.Date, "%m/%d/%y"), "%Y"))

sab_dens <- crab %>%
  group_by(year) %>%
  summarize(mean_dens = mean(dens), sd = sd(dens,na.rm=T))
beanplot(dens ~ year, data = sablefish, col = "steelblue")
lmsab <- lm(dens ~ year, data = sablefish)


