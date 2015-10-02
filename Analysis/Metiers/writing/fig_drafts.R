# make revenue versus number of trips

tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")

# figure 2a ----
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
met_names <- read.csv("/Users/efuller/1/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)

c.names = data.frame(metier = tolower(colnames(species_tab)[2:ncol(species_tab)]),stringsAsFactors = FALSE)
for(i in 1:nrow(c.names)){
  c.names$common_name[i] = paste0(met_names$Major_species[which(met_names$Metier == c.names$metier[i])], "\n",met_names$Major_gear[which(met_names$Metier == c.names$metier[i])])
}
colnames(species_tab)[2:ncol(species_tab)] <- c.names$common_name

# species common names
spid <- read.csv("/Users/efuller/1/CNH/processedData/catch/spid.csv",stringsAsFactors = FALSE)

r.names <- data.frame(spid = rownames(species_tab),stringsAsFactors = FALSE)
for(i in 1:nrow(r.names)){
  r.names$common_name[i] <- tolower(spid$common_name[which(tolower(spid$SPID)==r.names$spid[i])])
}
r.names$common_name <- gsub(" ","\n",r.names$common_name)
rownames(species_tab) <- r.names$common_name

heatmap(t(as.matrix(species_tab[,2:ncol(species_tab)])),col=c("white", brewer.pal(9,"Greys")), scale = "row", margins = c(8,8),cexRow = .75, cexCol = .75)


# figure 2b ----
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

# figure 2c----
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
colScale <- scale_colour_manual(name = "fishery",values = myColors)

# p <- ggplot(my_dat, aes(x = tdate, y = n.trips, colour=fishery)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')

# p + colScale +guides(colour=FALSE)

ggplot(my_dat, aes(x = tdate, y = n.trips)) + geom_bar(stat='identity') + facet_wrap(~fishery, ncol = 1, scales = "free_y") + theme_minimal() + xlab("date landed") + ylab('number of trips landed')


# figure 2d ----
# west coast map with landings
ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)

ex_space <- subset(tickets, metier.2010 %in% c("NET_1","TWS_1")) %>%
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


library(ggplot2)

ggplot(ex_space, aes(x = lat)) + geom_bar(aes(weight=revenue, fill = fishery),position="dodge")+ ylab("revenue ($)") + xlab("latitude")  + theme_classic() + scale_y_sqrt() + coord_flip() + scale_fill_manual(values = c("grey","black"))  

load("/Users/efuller/1/CNH/processedData/spatial/2_coastline.Rdata")
plot(WC, col = 'black',bg="transparent")

# figure 3 ----
# making histograms by management zone
# load ports with lat/lon
library(dplyr)
library(vegan)

ports <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
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
  
library(ggplot2)  

ves_summary$zone <- cut(x = ves_summary$mean.lat, breaks = rev(c(32, 36, 40, 43, 49)), labels = c("N. Cape Blanco", "Cape Blanco - Cape Mendocino", "Cape Mendocino - Point Sur", "S. Point Sur"), include.lowest = TRUE)

ggplot(subset(ves_summary[-which(is.na(ves_summary$mean.lat)),], diversity!=1), aes(x = diversity)) + geom_histogram(aes(y = ..count..)) + facet_wrap(~zone, ncol = 1,scales = "free_y") + theme_light() + xlab("effective shannon diversity of revenue") + ylab('number of vessels')

ves_summary$type <- ifelse(ves_summary$diversity==1, "specialist", "generalist")
ggplot(ves_summary[-which(is.na(ves_summary$mean.lat)),], aes(x = type)) + geom_bar() + facet_wrap(~zone, ncol = 1, scales = "free_y") + theme_light()

# figure 4 ----
# make port networks and measure degree, betwenness
library(igraph); library(reshape2); library(vegan); library(dplyr)

define_participationPlot <- function(year_choose, port=NA, restrict.trips=FALSE, restrict.revenue = FALSE,tickets = tickets.df, base_year = 2010,plot.yes=FALSE){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
  }
  
  if(nrow(yr_tickets)==0) return(NA)
  
  if(base_year == 2010){
    yr_tickets <- rename(yr_tickets, metier = metier.2010)
  }else{
    if(base_year == 2012){
      yr_tickets <- rename(yr_tickets, metier = metier.2012)
    }
  }
  
  m_by_v <- melt(yr_tickets, id.vars = c("metier","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  cast_mv <- dcast(m_by_v, metier~drvid, length) # calculate number of trips
  rownames(cast_mv) <- cast_mv$metier
  cast_mv <- cast_mv[,-1]
  # remove any metiers that aren't used at all
  
  # restrict to metiers responsible for at least 95% of trips 
  if(restrict.trips==TRUE){
    cumulatives <- cumsum(sort(rowSums(cast_mv)/sum(rowSums(cast_mv)),decreasing=T))
    r.f <- names(which(cumulatives<=.95))
    cast_mv <- cast_mv[r.f,]
    if(any(colSums(cast_mv)==0)) cast_mv <- cast_mv[,-which(colSums(cast_mv)==0)]
  }
  
  if(restrict.revenue==TRUE){
    trip_revs <- yr_tickets %>%
      group_by(metier) %>%
      summarize(trip.rev = sum(adj_revenue)) %>%
      arrange(-trip.rev)
    
    cumulatives <- cumsum(trip_revs$trip.rev)/sum(trip_revs$trip.rev)
    r.f <- trip_revs$metier[which(cumulatives<=.99)]
    cast_mv <- cast_mv[r.f,]
    if(any(colSums(cast_mv)==0)) cast_mv <- cast_mv[,-which(colSums(cast_mv)==0)]
  }
  
  hellinger_dist <- as.matrix(vegdist(decostand(cast_mv, "hellinger"), "euclidean"))
  hellinger_sim <- sqrt(2) - hellinger_dist #sqrt(2) is max of hellinger
  # are some rounding errors, so make anything < 0, 0
  # http://stackoverflow.com/questions/19444674/approximation-rounding-errors-in-r-in-simple-situations
  hellinger_sim[which(hellinger_sim < 0)] <- 0
  
  g <- graph.adjacency(hellinger_sim,weighted = TRUE, mode="undirected", diag = FALSE)
  if(restrict.trips==TRUE){
    V(g)$size <- rowSums(cast_mv)/sum(rowSums(cast_mv))*100
  }else{
    if(restrict.revenue==TRUE){
      V(g)$size <- trip_revs$trip.rev[which(trip_revs$metier %in% r.f)]/sum(trip_revs$trip.rev[which(trip_revs$metier %in% r.f)])*100
    }
  }
  
  l <- layout.fruchterman.reingold(g,niter=500)
  
  if(plot.yes == TRUE){
    plot(g, edge.width = E(g)$weight*30, layout = l, vertex.size = (V(g)$size), vertex.label.color = "black", vertex.label.family="sans", vertex.frame.color = NA)
  }
  return(g)
}

foo <- define_participationPlot(year_choose = 2009:2010, port = "NEW", tickets = tickets,restrict.revenue = TRUE)
mean(degree(foo))
mean(betweenness(foo))
edge_density(foo)
mean(page_rank(foo)$vector)
mean(closeness(foo))

sb <- define_participationPlot(year_choose = 2009:2010, port = "SB", tickets = tickets,restrict.revenue = TRUE)
edge_density(sb)
mean(degree(sb))
mean(betweenness(sb))
plot(sb, edge.width = E(foobar)$weight*30, vertex.frame.color = "grey", vertex.color='grey', vertex.label.family = 'sans', vertex.label.color = 'black')


crs <- define_participationPlot(year_choose = 2009:2010, port = "CRS", tickets = tickets, restrict.revenue = TRUE)
mean(degree(crs))
edge_density(crs)
mean(betweenness(crs))
plot(crs, edge.width = E(foobar)$weight*30, vertex.frame.color = "grey", vertex.color='grey', vertex.label.family = 'sans', vertex.label.color = 'black')


cos <- define_participationPlot(year_choose = 2009:2010, port = "COS", tickets = tickets, restrict.revenue = TRUE)
plot(cos, edge.width = E(foobar)$weight*30, vertex.frame.color = "grey", vertex.color='grey')

# figure 5 ----
diversities <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/bin/vessel_diversity_landings.RDS")

sub_data <- subset(diversities, hake == 0 & single.fishery_2010 == 0)


# individual model
lm2 <- lm(delta.eff.shannon_2010 ~  eff.shannon_2010 + ifq, sub_data)

# port model
port_df <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/bin/port_diversity_landings.RDS")

lm_port <- lm(delta.degree ~ has.ifq + mean_degree_prior, port_df)


# try sim 
library(arm)
lm2.sim <- sim(lm2)
coef.lm2.sim <- coef(lm2.sim)
sigma.lm2.sim <- sigma.hat(lm2.sim)

lm_port.sim <- sim(lm_port)
coef.lm_port.sim <- coef(lm_port.sim)
sigma.lm2.sim <- sigma.hat(lm_port.sim)
boxplot(cbind(coef.lm2.sim[,3], coef.lm_port.sim[,2]), bty = "n", names =c("vessel level","port level"), ylab = "IFQ effect", col = 'grey')
abline(h = 0, lwd = 2)
