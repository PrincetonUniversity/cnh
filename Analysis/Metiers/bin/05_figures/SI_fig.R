# plot composition of fisheries
# S1
# species table
library(reshape2)
library(dplyr)
tickets <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/04_data_output/vessel_landings_data.RDS")
met_summary <- tickets %>%
  group_by(metier.2010, modified) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(metier.2010) %>%
  mutate(total_rev = sum(revenue), per.rev = revenue/total_rev)

# subset to top 30 by revenue
met_rev <- unique(met_summary[,c("metier.2010","total_rev")])
met_rev <- met_rev[order(-met_rev$total_rev),]


species_melt <- melt(met_summary, id.vars = c("modified","metier.2010"), measure.vars = "per.rev")
species_melt <- subset(species_melt, metier.2010 %in% met_rev$metier.2010[1:10])
species_tab <- dcast(species_melt, modified ~ metier.2010, fun.aggregate = sum)

# remove species that have < 10% across all fisheries
library(RColorBrewer)
species_tab <- species_tab[-which(rowSums(species_tab[,2:ncol(species_tab)])<.05),]
rownames(species_tab) <- tolower(species_tab$modified)

# add fishery names and species common names
met_names <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)

c.names = data.frame(metier = tolower(colnames(species_tab)[2:ncol(species_tab)]),stringsAsFactors = FALSE)
for(i in 1:nrow(c.names)){
  c.names$common_name[i] = paste0(met_names$Major_species[which(met_names$Metier == c.names$metier[i])], "\n",met_names$Major_gear[which(met_names$Metier == c.names$metier[i])])
}
colnames(species_tab)[2:ncol(species_tab)] <- c.names$common_name

# species common names
spid <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/spid.csv",stringsAsFactors = FALSE)

r.names <- data.frame(spid = rownames(species_tab),stringsAsFactors = FALSE)
for(i in 1:nrow(r.names)){
  r.names$common_name[i] <- tolower(spid$common_name[which(tolower(spid$SPID)==r.names$spid[i])])
}
r.names$common_name <- gsub(" ","\n",r.names$common_name)
rownames(species_tab) <- r.names$common_name

pdf("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/05_figures/S1a.pdf",width = 6, height = 6)
heatmap(t(as.matrix(species_tab[,2:ncol(species_tab)])),col=c("white", brewer.pal(9,"Greys")), scale = "row", margins = c(8,8),cexRow = .75, cexCol = .75)
dev.off()
# figure S2
# effort and revenue plot
library(dplyr)
met_summary <- tickets %>%
  filter(year %in% c(2009,2010)) %>%
  group_by(metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T),
            n.trips = length(unique(trip_id)))

met_summary$paint <- "black"

met_summary$cex <- .6
met_summary$cex[which(met_summary$metier.2010=="TWL_1")] <- 1
met_summary$cex[which(met_summary$metier.2010=="POT_1")] <- 1
met_summary$cex[which(met_summary$metier.2010=="TWS_1")] <- 1
met_summary$cex[which(met_summary$metier.2010=="MSC_1")] <- 1
met_summary$cex[which(met_summary$metier.2010=="HKL_2")] <- 1
par(cex.axis = .8, cex.lab = .9 )
with(met_summary, plot(n.trips, revenue, type="p", bty="n",pch=19, xlab = "number of trips landed (2009-2010)", ylab="total revenue (2009-2010)",log = "xy", col = paint, cex = met_summary$cex, xlim=c(1,500000), ylim = c(1,1e9)))

text(met_summary$n.trips[which(met_summary$metier.2010=="TWL_1")], met_summary$revenue[which(met_summary$metier.2010=="TWL_1")], labels = "dover sole\nroller trawl", col = "black", cex = .8, pos = 3)

text(met_summary$n.trips[which(met_summary$metier.2010=="TWS_1")], met_summary$revenue[which(met_summary$metier.2010=="TWS_1")], labels = "pink shrimp\ntrawl", col = "black", cex = .8, pos = 2)

text(met_summary$n.trips[which(met_summary$metier.2010=="POT_1")], met_summary$revenue[which(met_summary$metier.2010=="POT_1")], labels = "dungenness\ncrab pots", col = "black", cex = .8, pos = 4)

text(met_summary$n.trips[which(met_summary$metier.2010=="MSC_1")], met_summary$revenue[which(met_summary$metier.2010=="MSC_1")], labels = "red urchin\ndiving", col = "black", cex = .8, pos = 4)

text(met_summary$n.trips[which(met_summary$metier.2010=="HKL_2")], met_summary$revenue[which(met_summary$metier.2010=="HKL_2")], labels = "black rockfish\nhook & line", col = "black", cex = .8, pos = 4)