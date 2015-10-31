# making figures
rm(list=ls())

# load data ----
tickets <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/data_output/diversity_landings_data.RDS")

# figure 2 ----
# making histograms by management zone
# load ports with lat/lon
library(dplyr)
library(vegan)

ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

# look at each vessel, calculate diversity 2009-2010, and average latitude
ves_summary <- tickets %>%
  filter(year < 2011) %>%
  group_by(drvid, metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(drvid) %>%
  summarize(diversity = exp(diversity(revenue, index = 'shannon')))

lat_landed <- tickets %>%
  filter(year < 2011) %>%
  select(trip_id, drvid, pcid) %>%
  distinct() %>%
  left_join(ports) %>%
  group_by(drvid) %>%
  summarize(mean.lat = mean(lat, na.rm = T))

ves_summary <- merge(ves_summary, lat_landed)
ves_summary$zone <- cut(x = ves_summary$mean.lat, 
                        breaks = rev(c(32, 36, 40, 43, 49)), 
                        labels = c("N. Cape Blanco", 
                                   "Cape Blanco - Cape Mendocino", 
                                   "Cape Mendocino - Point Sur", 
                                   "S. Point Sur"), include.lowest = TRUE)
ves_summary$type <- ifelse(ves_summary$diversity==1, "specialist", "generalist")

# plot
library(ggplot2)  
library(cowplot)

coastwide <- ggplot(ves_summary[-which(is.na(ves_summary$mean.lat)),], aes(x = type)) + geom_bar() + ylab("number of vessels") + background_grid(major = "xy", minor = "none")

by_zone <- ggplot(ves_summary[-which(is.na(ves_summary$mean.lat)),], aes(x = type)) + geom_bar() + facet_wrap(~zone, ncol = 1, scales = "free_y") + ylab('number of vessels') + background_grid(major = "xy", minor = "none")

div_hist <- ggplot(subset(ves_summary[-which(is.na(ves_summary$mean.lat)),], diversity!=1), aes(x = diversity)) + geom_histogram(aes(y = ..count..)) + facet_wrap(~zone, ncol = 1,scales = "free_y") + xlab("effective shannon diversity of revenue") + ylab('number of vessels') + background_grid(major = "xy", minor = "none")

all_plot <- plot_grid(coastwide, by_zone, div_hist, labels = c("A","B","C"), ncol = 3, rel_widths = c(1,1,1.75))

save_plot("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/figures/fig_2.pdf", 
          all_plot,base_height = 8, base_width = 12)

# figure 3 ----
# make port networks and measure degree, betwenness
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/data_output/port_diversity_network_metrics.RDS")

descrp <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)

# each major gear is a color, and then color ramp within gear between light and dark color
# pot = reds
# tws = pinks
# tls = yellows
# msc = purples
# hkl = greens
# twl = oranges
# net = blues

descrp$paint <- NA
library(RColorBrewer)

# find number of pot gears
n.gear = length(grep("pot",descrp$Metier))
paint = rev(colorRampPalette(brewer.pal(9, "Reds"))(n.gear))
descrp$paint[grep("pot",descrp$Metier)] <- paint

n.gear = length(grep("tws",descrp$Metier))
paint = colorRampPalette(c("#FA9FB5","#E7298A"))(n.gear)
descrp$paint[grep("tws",descrp$Metier)] <- paint

n.gear = length(grep("tls",descrp$Metier))
paint = colorRampPalette(c("#FFEDA0","#FED976"))(n.gear)
descrp$paint[grep("tls",descrp$Metier)] <- paint

n.gear = length(grep("msc",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Purples"))(n.gear)
descrp$paint[grep("msc",descrp$Metier)] <- paint

n.gear = length(grep("hkl",descrp$Metier))
paint = rev(colorRampPalette(brewer.pal(9, "Greens"))(n.gear))
descrp$paint[grep("hkl",descrp$Metier)] <- paint

n.gear = length(grep("twl",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Oranges"))(n.gear)
descrp$paint[grep("twl",descrp$Metier)] <- paint

n.gear = length(grep("net",descrp$Metier))
paint = colorRampPalette(brewer.pal(9, "Blues"))(n.gear)
descrp$paint[grep("net",descrp$Metier)] <- paint

library(reshape2);library(igraph)
define_participationPlot <- function(year_choose, port=NA, restrict=TRUE, tickets = tickets.df){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier.2010","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  if(nrow(m_by_v)<2) return(NA)
  cast_mv <- dcast(m_by_v, metier.2010~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier.2010
  cast_mv <- cast_mv[,-1, drop=FALSE]
  
  # want to calculate total number of vessels that do each fishery, and then 
  # make connections be what proportion does both
  # total number is row sums. And percentage that does both, relative to base 
  # nodes 
  
  # so symmetric matrix, of metiers, entries number of boats do both. 
  # Divide rows by row/sums to get directon. then each entry is the proprotion 
  # of boats that do both, relative to row-metier.
  
  met_mat <- matrix(data = 0, nrow = nrow(cast_mv), ncol = nrow(cast_mv))
  colnames(met_mat) <- rownames(cast_mv)
  rownames(met_mat) <- rownames(cast_mv)
  for(i in 1:nrow(met_mat)){
    for(j in 1:ncol(met_mat)){
      met_mat[i,j] <- sum(apply(cast_mv, MARGIN = 2, function(x) ifelse(x[i] & x[j]>0, 1, 0)))
    }
  }
  
  # make directed network, divide by diagonals
  met_mat <- met_mat/diag(met_mat)
  
  # put diagonals back in - number of vessels
  diag(met_mat) <- apply(X = cast_mv, MARGIN = 1, function(x) length(which(x>0)))
  
  # make network
  g <- graph.adjacency(met_mat,weighted = TRUE, mode="directed", diag = FALSE)
  V(g)$size <- diag(met_mat)
  
  contain_metier <- which(descrp$Metier %in% tolower(V(g)$name))
  
  cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
  # add "multi" for metiers that are
  cn$cn <- ifelse(descrp$multi_species[contain_metier]=="yes", paste0("multi sp. ",cn$cn), cn$cn)
  cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
  
  g_s <- g
  V(g_s)$name <- cn$cn
  
  V(g_s)$color <- cn$paint
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  plot(g_s, edge.width = E(g_s)$weight*3, layout = l, vertex.size = log(V(g_s)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
  
  return(list(g_s, cast_mv))
}
port_df <- port_df[order(port_df$ic_pre, decreasing = TRUE),]

pdf("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/figures/fig_3c.pdf",width = 16, height = 8)
  par(bg="transparent", cex = .75,cex=1.2)
  barplot(port_df$ic_pre, bor = F, ylab = "average fishery connectance", xlab="port")
dev.off()

# aux ploting - drop nodes with < 3 vessels
gp <- function(g_s){
  g=delete.vertices(g_s,which(V(g_s)$size<3))
  plot(g, edge.width = E(g)$weight*3, layout = layout.fruchterman.reingold, vertex.size = log(V(g)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, edge.color = "black")
}

sb <- define_participationPlot(year=2009:2010, port = "SB", tickets = tickets)
erk <- define_participationPlot(year=2009:2010, port = "ERK", tickets = tickets)

crs <- define_participationPlot(year=2009:2010, port = "CRS", tickets = tickets)
new <- define_participationPlot(year=2009:2010, port = "NEW", tickets = tickets)
ast <- define_participationPlot(year=2009:2010, port = "NEW", tickets = tickets)
mnt <- define_participationPlot(year=2009:2010, port = "MNT", tickets = tickets)
mos <- define_participationPlot(year=2009:2010, port = "MOS", tickets = tickets)
wpt <- define_participationPlot(year=2009:2010, port = "WPT", tickets = tickets)
nea <- define_participationPlot(year=2009:2010, port = "NEA", tickets = tickets)
orf <- define_participationPlot(year=2009:2010, port = "ORF", tickets = tickets)

load("/Users/efuller/Desktop/CNH/processedData/spatial/2_coastline.Rdata")
par(bg="transparent")
plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="NEA")], ports$lat[which(ports$pcid=="NEA")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="WPT")], ports$lat[which(ports$pcid=="WPT")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="AST")], ports$lat[which(ports$pcid=="AST")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="NEW")], ports$lat[which(ports$pcid=="NEW")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="CRS")], ports$lat[which(ports$pcid=="CRS")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="ERK")], ports$lat[which(ports$pcid=="ERK")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="MNT")], ports$lat[which(ports$pcid=="MNT")],col='dodgerblue',pch=19)

plot(WC, col = 'grey50',bg="transparent", axes=FALSE,bor=F)
points(ports$lon[which(ports$pcid=="SB")], ports$lat[which(ports$pcid=="SB")],col='dodgerblue',pch=19)

pdf("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/figures/fig_3a.pdf",width = 8, height = 8)
  par(bg="transparent")
  g=delete.vertices(sb[[1]],which(V(sb[[1]])$size<3))
  plot(g, edge.width = E(g)$weight*1, 
       layout = layout.circle, vertex.size = log(V(g)$size)*2, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, 
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = "black")
dev.off()

pdf("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/figures/fig_3b.pdf",width = 8, height = 8)
  par(bg="transparent")
  g=delete.vertices(erk[[1]],which(V(erk[[1]])$size<3))
  plot(g, edge.width = E(g)$weight*1, 
       layout = layout.circle, vertex.size = log(V(g)$size)*2, 
       vertex.label.color = "black", vertex.label.cex = .5, 
       vertex.label.family="sans", vertex.frame.color = NA, 
       edge.curved=TRUE, edge.arrow.size = .05, edge.color = "black")
  dev.off()

# figure 4 ----
# making linear regression

diversities <- readRDS("/users/efuller/Desktop/CNH/Analysis/Metiers/bin/data_output/vessel_diversity_landings.RDS")
sub_data <- subset(diversities, hake == 0)

# individual model
lm1 <- lm(delta.eff.shannon_2010 ~ eff.shannon_2010, sub_data)
lm2 <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq, sub_data)
lm3 <- lm(delta.eff.shannon_2010 ~ ifq, sub_data)
lm4 <- lm(delta.eff.shannon_2010 ~ ifq + eff.shannon_2010 + ifq*eff.shannon_2010, sub_data)
# port model
port_df <- readRDS("/Users/efuller/Desktop/CNH/Analysis/Metiers/bin/data_output/port_diversity_network_metrics.RDS")

# look before and after in ports
t.test(port_df$ic_pre,port_df$ic_post)
with(subset(port_df, ifq==1), t.test(ic_pre, ic_post))

lm_port1 <- lm(ic_delta ~ ic_pre, port_df)
lm_port <- lm(ic_delta ~  ic_pre + ifq, port_df)
lm_port3 <- lm(ic_delta ~ ifq, port_df)


# sim to get effects 
library(arm)
lm2.sim <- sim(lm2,n=10000)
coef.lm2.sim <- coef(lm2.sim)
sigma.lm2.sim <- sigma.hat(lm2.sim)

lm_port.sim <- sim(lm_port,	n=10000)
coef.lm_port.sim <- coef(lm_port.sim)
sigma.lm2.sim <- sigma.hat(lm_port.sim)

ifq_effect <- as.data.frame(cbind(coef.lm2.sim[,3], coef.lm_port.sim[,3]))
colnames(ifq_effect) <- c("vessel","port")
ifq_effect <- gather(ifq_effect)
effect.size <- ggplot(ifq_effect, aes(x=key, y = value)) + geom_boxplot(outlier.size = 0, fill = "grey") + geom_hline(yinterecept = 0) + xlab("level") + ylab("change in diversity")

save_plot("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/figures/fig_4.pdf", effect.size)

boxplot(coef.lm2.sim[,2],outline=FALSE,ylim=c(-3,3)) + abline(h=0,lwd=2)
boxplot(coef.lm_port.sim[,3],outline=FALSE,ylim=c(-3,3)) + abline(h=0,lwd=2)

# look at intercepts
boxplot(cbind(coef.lm2.sim[,1], coef.lm_port.sim[,1]),bty='n',names=c('vessel level', 'port level'), ylab = 'Change in diversity', col='grey',ylim=c(-3,3), outline=FALSE, bty="n")
abline(h = 0, lwd = 2)

# try dropping mean_degree prior since not large effect
lm_port2 <- lm(delta.degree ~ has.ifq, port_df)

# supplementary figures ----
# S1
# species table
library(reshape2)
library(dplyr)
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
met_names <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)

c.names = data.frame(metier = tolower(colnames(species_tab)[2:ncol(species_tab)]),stringsAsFactors = FALSE)
for(i in 1:nrow(c.names)){
  c.names$common_name[i] = paste0(met_names$Major_species[which(met_names$Metier == c.names$metier[i])], "\n",met_names$Major_gear[which(met_names$Metier == c.names$metier[i])])
}
colnames(species_tab)[2:ncol(species_tab)] <- c.names$common_name

# species common names
spid <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/catch/spid.csv",stringsAsFactors = FALSE)

r.names <- data.frame(spid = rownames(species_tab),stringsAsFactors = FALSE)
for(i in 1:nrow(r.names)){
  r.names$common_name[i] <- tolower(spid$common_name[which(tolower(spid$SPID)==r.names$spid[i])])
}
r.names$common_name <- gsub(" ","\n",r.names$common_name)
rownames(species_tab) <- r.names$common_name

heatmap(t(as.matrix(species_tab[,2:ncol(species_tab)])),col=c("white", brewer.pal(9,"Greys")), scale = "row", margins = c(8,8),cexRow = .75, cexCol = .75)

# figure S2
# effort and revenue plot
library(dplyr)
met_summary <- tickets %>%
  filter(year %in% c(2009,2010)) %>%
  group_by(metier.2010) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T),
            n.trips = length(unique(trip_id)))

met_summary$paint <- "black"
# met_summary$paint[which(met_summary$metier.2010=="TWL_1")] <- "#859900"
# met_summary$paint[which(met_summary$metier.2010=="POT_1")] <- "#cb4b16"
# met_summary$paint[which(met_summary$metier.2010=="TWS_1")] <- "#2aa198"
# met_summary$paint[which(met_summary$metier.2010=="MSC_1")] <- "#6c71c4"
# met_summary$paint[which(met_summary$metier.2010=="HKL_2")] <- "#268bd2"

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

# figure S3
# time series of landings
by_day <- tickets %>%
  filter(year < 2011) %>%
  group_by(tdate, metier.2010) %>%
  summarize(n.trips = length(unique(trip_id)))

by_day$tdate <- as.Date(by_day$tdate, format = "%d-%b-%y")
by_day$fishery <- ifelse(by_day$metier.2010 == "HKL_2", "black rockfish hook & line",
                  ifelse(by_day$metier.2010 == "NET_1", "market squid seine",
                  ifelse(by_day$metier.2010 == "TWL_1", "groundfish trawl",
                  ifelse(by_day$metier.2010 == "POT_1","dungeness crab pots",
                  ifelse(by_day$metier.2010 == "TWS_1", "pink shrimp trawl", NA)))))

my_dat <-as.data.frame(subset(by_day, metier.2010 %in% c("HKL_2","NET_1","TWS_1","POT_1","TWL_1")))
my_dat$fishery <- factor(my_dat$fishery)
myColors <- toupper(c("#268bd2","#cb4b16","#859900","#2aa198","#6c71c4"))
names(myColors) <- levels(my_dat$fishery)
library(ggplot2)
colScale <- scale_colour_manual(name = "fishery",values = myColors)

# p <- ggplot(my_dat, aes(x = tdate, y = n.trips, colour=fishery)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')

# p + colScale +guides(colour=FALSE)

ggplot(my_dat, aes(x = tdate, y = n.trips)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')


# figure S4
# west coast map with landings
ports <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

ex_space <- subset(tickets, metier.2010 %in% c("TWL_1","TWS_1")) %>%
  filter(year < 2011) %>%
  group_by(metier.2010, trip_id) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T), pcid = unique(pcid)) %>%
  left_join(ports)

ex_space$fishery <- ifelse(ex_space$metier.2010 == "HKL_2", "black rockfish hook & line",
                    ifelse(ex_space$metier.2010 == "NET_1", "squid seine",
                    ifelse(ex_space$metier.2010 == "TWL_1", "groundfish trawl",
                    ifelse(ex_space$metier.2010 == "POT_1","dungeness crab pots",
                    ifelse(ex_space$metier.2010 == "TWS_1", "pink shrimp trawl", NA)))))

ex_space$fishery <- as.factor(ex_space$fishery)

ggplot(ex_space, aes(x = lat)) + geom_bar(aes(weight=revenue, fill = fishery),position="dodge",binwidth=1)+ ylab("revenue ($)") + xlab("latitude")  + theme_classic() + scale_y_sqrt()  + scale_fill_manual(values = c("grey40","grey80")) +  scale_y_continuous(trans = "reverse") + coord_flip() + theme(plot.background =  element_rect(fill="transparent"), panel.background = element_rect(fill="transparent"), legend.background = element_rect(fill='transparent'))

load("/Users/efuller/Desktop/CNH/processedData/spatial/2_coastline.Rdata")
plot(WC, col = 'black',bg="transparent", axes=TRUE)

