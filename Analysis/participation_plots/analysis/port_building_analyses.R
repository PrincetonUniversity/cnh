# looking at crab centrality

# load port networks ----

library(dplyr); library(reshape2); library(igraph); library(tidyr)
vessel_landings <- readRDS("/Users/efuller/Desktop/CNH/processedData/catch/1_cleaningData/tickets.RDS")
ports <- read.csv("/Users/efuller/Desktop/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
colnames(ports) <- tolower(colnames(ports))

# top ten fisheries
topten <- vessel_landings %>%
  group_by(metier.2010) %>%
  summarize(revenue = sum(adj_revenue), lbs = sum(landed_wt), 
            trips = length(unique(trip_id)), vessels = length(unique(drvid))) %>%
  arrange(-revenue, -lbs, -trips, -vessels) %>%
  ungroup() %>%
  mutate(c_rev = cumsum(as.numeric(revenue))/sum(as.numeric(revenue)),
         c_lbs = cumsum(as.numeric(lbs))/sum(as.numeric(lbs)),
         c_trips = cumsum(trips)/sum(trips),
         c_vessels = cumsum(vessels)/sum(vessels))

other_ports <- ports$pcid[grep("other",ports$name)]

topten_ports <- vessel_landings %>%
  filter(!(pcid %in% other_ports)) %>%
  group_by(pcid) %>%
  summarize(revenue = sum(adj_revenue), lbs = sum(landed_wt), 
            trips = length(unique(trip_id)), vessels = length(unique(drvid))) %>%
  arrange(-revenue, -lbs, -trips, -vessels) %>%
  ungroup() %>%
  mutate(c_rev = cumsum(as.numeric(revenue))/sum(as.numeric(revenue)),
         c_lbs = cumsum(as.numeric(lbs))/sum(as.numeric(lbs)),
         c_trips = cumsum(trips)/sum(trips),
         c_vessels = cumsum(vessels)/sum(vessels))

topten %>% dplyr::select(metier.2010, starts_with("c_")) %>% 
  gather(type,cumulative,-metier.2010) %>%
  ggplot(aes(x = reorder(metier.2010, cumulative, median), y = cumulative, color = type)) + 
  geom_point() + xlab("fishery") + ylab("cumulative percentage")

# drop to top 22 ports
vessel_landings <- subset(vessel_landings, pcid %in% topten_ports$pcid[1:28])

# define participation network ----
# undirected, based on number of vessels that do both fisheries. 
# size is number of vessels
define_participationPlot <- function(year_choose, port=NA, restrict=FALSE, tickets = tickets.df){
  library(dplyr); library(reshape2); library(igraph); library(tidyr)
  descrp <- read.csv("/Users/efuller/Desktop/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)
  
  descrp$paint <- NA
  library(RColorBrewer)
  descrp$Metier <- tolower(descrp$Metier)
  
  # find number of pot gears
  n.gear = length(grep("pot",tolower(descrp$Metier)))
  paint = rev(colorRampPalette(brewer.pal(9, "Reds"))(n.gear))
  descrp$paint[grep("pot",tolower(descrp$Metier))] <- paint
  
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
  if(any(is.na(port))){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(any(!is.na(port))){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid %in% port),]
  }
  
  if(restrict){ # drop nodes with < 3 vessels and 
    # retain only metiers responsible for >99% of revenue
    ninty <- yr_tickets %>%
      group_by(metier.2010) %>%
      summarize(rev = sum(adj_revenue), nves = length(unique(drvid))) %>%
      arrange(-rev) %>%
      ungroup() %>%
      mutate(c_rev = cumsum(rev)/sum(rev)) %>%
      filter(c_rev <= min(c_rev[which(c_rev>.95)]) & nves >2)
    
    yr_tickets <- subset(yr_tickets, metier.2010 %in% ninty$metier.2010)
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
  
  # make network
  g <- graph.adjacency(met_mat,weighted = TRUE, mode="undirected", diag = FALSE)
  V(g)$size <- diag(met_mat)
  
  contain_metier <- which(descrp$Metier %in% tolower(V(g)$name))
  
  cn <- data.frame(cn = paste(descrp$Major_species[contain_metier], descrp$Major_gear[contain_metier],sep="\n"), metier = descrp$Metier[contain_metier], paint = descrp$paint[contain_metier], stringsAsFactors = FALSE)
  cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
  
  g_s <- g
  V(g_s)$common_name <- cn$cn
  
  V(g_s)$color <- cn$paint
  
  
  
  #l <- layout.fruchterman.reingold(g_s,niter=500)
  
  #plot(g_s, edge.width = log(E(g_s)$weight), vertex.size = log(V(g_s)$size)*3, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
  
  return(g_s)
}

# calculate networks for ports ----
port_list <- list()
prts <- unique(vessel_landings$pcid)
for(p in 1:length(prts)){
  port_list[[p]] <- define_participationPlot(year_choose = 2009:2013, 
                                             port = prts[p], restrict = TRUE, 
                                             tickets = vessel_landings)
}

names(port_list) <- prts

# calculate networks for states ----
state_list <- list()
sts <- split(ports$pcid, ports$state)
for(s in 1:length(sts)){
  state_list[[s]] <- define_participationPlot(year_choose = 2009:2013, 
                                              port = sts[[s]], restrict = TRUE,
                                              tickets = vessel_landings)
}

# calculate network for coast ----
coastwide <- define_participationPlot(year_choose = 2009:2013, port = NA, 
                                      restrict = TRUE, 
                                      tickets = vessel_landings)

# plot linkage density for ports----
port_df <- data.frame(ld = sapply(port_list, function(x) length(E(x))/length(V(x))),
                      pcid = names(port_list),stringsAsFactors = FALSE)
port_df$paint ="normal"
port_df$paint[port_df$pcid %in% c("SB","ORF","CRS")] <- "example"

ggplot(subset(port_df, !is.nan(ld)), aes(x = reorder(pcid, ld), fill=factor(paint), y = ld)) + 
  geom_bar(stat='identity') + xlab("port") + 
  ylab("port connectance") + scale_fill_manual(values = c("grey20","grey50")) + theme_pander() 
ggsave("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_ld.pdf",width = 13, height = 5)

which(names(port_list) %in% c("SB","ORF","CRS"))
png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_orf.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = port_list[[6]]
plot(g, edge.width = E(g)$weight*.45, 
     layout = layout.circle, vertex.size = log(V(g)$size)*5, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
dev.off()

png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_crs.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = port_list[[11]]
plot(g, edge.width = E(g)$weight, 
     layout = layout.circle, vertex.size = log(V(g)$size)*5, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
dev.off()

png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_sb.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = port_list[[15]]
plot(g, edge.width = E(g)$weight*.5, 
     layout = layout.circle, vertex.size = log(V(g)$size)*5, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.25))
dev.off()

# plot linkage density for states----
state_df <- data.frame(ld = sapply(state_list, function(x) length(E(x))/length(V(x))),
                      state = names(sts) ,stringsAsFactors = FALSE)

ggplot(state_df, aes(x = reorder(state, ld), y = ld)) + 
  geom_bar(stat='identity') + xlab("state") + 
  ylab("port connectance") + scale_fill_manual(values = c("grey20","grey50")) + theme_pander() 

ggsave("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_3_ld_state.pdf",width = 10, height = 8)

png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_ca.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = state_list[[1]]
plot(g, edge.width = log(E(g)$weight)*.5, 
     layout = layout.circle, vertex.size = log(V(g)$size)*2, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("white",.5))
dev.off()

png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_or.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = state_list[[2]]
plot(g, edge.width = log(E(g)$weight), 
     layout = layout.circle, vertex.size = log(V(g)$size)*2, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("white",.5))
dev.off()

png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_wa.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = state_list[[3]]
plot(g, edge.width = log(E(g)$weight)*2, 
     layout = layout.circle, vertex.size = log(V(g)$size)*3, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("white",.5))
dev.off()

# plot linkage density for coast ----
png("/Users/efuller/Desktop/CNH/Analysis/participation_plots/analysis/fig_2_coastwide.png",
    width = 4, height = 4, res = 300, units = "in")
par(bg="transparent")
g = coastwide
plot(g, edge.width = log(E(g)$weight)*.5, 
     layout = layout.circle, vertex.size = log(V(g)$size), 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=TRUE, edge.arrow.size = .05, edge.color = alpha("black",.2))
dev.off()

# calculate centrality for each fishery for ports ----
net_stats <- function(g, type, name){
  ew = mean(E(g)$weight,na.rm=T)/E(g)$weight
  node_strength = strength(g)/sum(E(g)$weight)
  eigen = eigen_centrality(g,weights = ew)$vector
  btwn = betweenness(g, directed=FALSE, weights = ew, normalized = TRUE)
  net_stats = as.data.frame(cbind(node_strength, eigen, btwn))
  net_stats$metier = rownames(net_stats)
  net_stats$pcid = name
  rownames(net_stats) <- NULL
  net_stats$type = type
  return(net_stats)
}
# geom_boxplot for ports

port_between <- list()
for(p in 1:length(port_list)){
  if(length(V(port_list[[p]]))==0){next}else{
    port_between[[p]] <- net_stats(g = port_list[[p]], 
                                   type = 'port',
                                   name = names(port_list)[p])
  }
} 
# not sure why errors, think it's from ew but shouldn't run if if is FALSE.. 

port_long <- do.call(rbind, port_between) %>%
  gather(net_stat, value,-metier, -type,-pcid ) %>%
  left_join(dplyr::select(ports, pcid, state))

ggplot(subset(port_long, net_stat=='node_strength' & !is.na(value)),
       aes(x = reorder(metier, value, median), y = value)) + 
  geom_boxplot(fill='grey') + xlab("fishery") + 
  theme(axis.text =  element_text(size=10, angle=90)) + ylab("node strength")
ggsave("Analysis/participation_plots/analysis/nodestrength.pdf")

ggplot(subset(port_long, net_stat=='btwn' & !is.na(value)),
       aes(x = reorder(metier, value, median), y = value)) + 
  geom_boxplot(fill='grey') + xlab("fishery") + ylab("betweeness") +
  theme(axis.text =  element_text(size=10, angle=90))
ggsave("Analysis/participation_plots/analysis/btwn.pdf")

# calculate centrality for each fishery for states ----
# geom_boxplot for ports
state_between <- list()
for(s in 1:length(state_list)){
    state_between[[s]] <- net_stats(g = state_list[[s]], 
                                   type = 'state')
} 

state_long <- do.call(rbind, state_between) %>%
  gather(net_stat, value,-metier, -type ) 

ggplot(subset(state_long, net_stat=='node_strength' & !is.na(value)),
       aes(x = reorder(metier, value, median), y = value)) + 
  geom_boxplot(fill='grey') + xlab("fishery") + 
  theme(axis.text =  element_text(size=10, angle=90)) + ylab("node strength")
ggsave("Analysis/participation_plots/analysis/nodestrength_state.pdf")

ggplot(subset(state_long, net_stat=='btwn' & !is.na(value)),
       aes(x = reorder(metier, value, median), y = value)) + 
  geom_boxplot(fill='grey') + xlab("fishery") + ylab("betweeness") +
  theme(axis.text =  element_text(size=10, angle=90))
ggsave("Analysis/participation_plots/analysis/btwn_state.pdf")

# calculate centrality for each fishery for coastwide ----
# geom_boxplot for ports
coast_between <- net_stats(g = state_list[[s]], type = 'coastwide')
coast_long <- coast_between %>%
  gather(net_stat, value,-metier, -type ) 

ggplot(subset(coast_long, net_stat == "node_strength"), 
       aes(coast_long, x = reorder(metier, value), y = value)) +
  geom_point() + xlab("fishery") 
ggsave("Analysis/participation_plots/analysis/nodestrength_coast.pdf")

ggplot(subset(coast_long, net_stat == "btwn"), 
       aes(coast_long, x = reorder(metier, value), y = value)) +
  geom_point() + xlab("fishery") 
ggsave("Analysis/participation_plots/analysis/btwn_coast.pdf")

# modularity ----
# just doing top ten ports
possible_shapes <- c("circle","square","csquare",
                     "rectangle","crectangle","vrectangle")
sub_ports <- port_list[which(names(port_list) %in% topten_ports$pcid[1:10])]

pdf("Analysis/participation_plots/analysis/modules.pdf", width = 4, height = 8)
par(mfrow=c(5,2),mai=rep(0,4))
for(i in 1:length(sub_ports)){
g = port_list[[i]]
wc <- cluster_walktrap(g)
V(g)$shape <- possible_shapes[membership(wc)]
plot(g, edge.width = E(g)$weight*.1, 
     vertex.size = log(V(g)$size)*5, 
     vertex.label.color = "black", vertex.label.cex = .5, 
     vertex.label.family="sans", vertex.frame.color = NA, vertex.label="",
     edge.curved=FALSE, edge.arrow.size = .05, edge.color = alpha("black",.25))
}
dev.off()