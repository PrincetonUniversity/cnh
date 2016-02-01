source("Analysis/ses_network/figures/participation_plots/network_functions.R")
tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")
library(dplyr)

# find list of ports for which we keep at least 95% of revenue
# after dropping metiers < 3 boats. Also it has to have crab

all_ports <- tickets %>%
  group_by(pcid) %>%
  dplyr::select(pcid, drvid, metier.2010, trip_id, adj_revenue)%>%
  group_by(pcid, metier.2010, drvid, trip_id) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(pcid, metier.2010) %>%
  summarize(n_boats = length(unique(drvid)), revenue = sum(revenue)) %>%
  group_by(pcid) %>%
  mutate(total_rev = sum(revenue)) %>%
  filter(n_boats >= 3) %>%
  summarize(percent_rev = sum(revenue)/unique(total_rev), 
            n.fisheries = length(unique(metier.2010)), 
            has_crab = ifelse(any(metier.2010=="POT_1"), "yes","no")) %>%
  filter(percent_rev > .95, has_crab == "yes")

# build plots and save locally ----

port_list <- list()
for(i in 1:nrow(all_ports)){
  port_list[[i]] <- participation_plots(port_choose = all_ports$pcid[i],
                                        years=2009:2013, cool_plot=TRUE, 
                                        save = TRUE)[[1]]
  names(port_list[[i]]) <- all_ports$pcid[i]
}
# get some network statistics
# number of nodes
# average interaction strength, 
# connectance, as I’ve defined it (will average connectance strength (average edgweight)
# something about modularity

# every other list entry is an igraph object

# getting statistics
all_ports$average_int <- NA
all_ports$modularity <- NA
all_ports$connectance <- NA
all_ports$pot_1centrality <- NA
for(i in 1:length(port_list)){
  all_ports$average_int[i] <- mean(E(port_list[[i]])$weight)
  all_ports$modularity[i] <- length(cluster_walktrap(port_list[[i]]))
  all_ports$connectance[i] <- sum(E(port_list[[i]])$weight)/length(V(port_list[[i]]))
  all_ports$pot_1centrality[i] <- closeness(port_list[[i]],vids="dungeness crab\ncrab pot")
}

# adding coastwide
cw <- participation_plots(port_choose = "coastwide",
                          years=2009:2013, cool_plot=TRUE, 
                          save = TRUE)[[1]]
all_ports <- rbind(all_ports, rep(NA, ncol(all_ports)))


coastwide <- tickets %>%
  dplyr::select(drvid, metier.2010, trip_id, adj_revenue)%>%
  group_by(metier.2010, drvid, trip_id) %>%
  summarize(revenue = sum(adj_revenue, na.rm = T)) %>%
  group_by(metier.2010) %>%
  summarize(n_boats = length(unique(drvid)), revenue = sum(revenue)) %>%
  mutate(total_rev = sum(revenue)) %>%
  filter(n_boats >= 3) %>%
  summarize(percent_rev = sum(revenue)/unique(total_rev), 
            n.fisheries = length(unique(metier.2010)), 
            has_crab = ifelse(any(metier.2010=="POT_1"), "yes","no")) %>%
  filter(percent_rev > .95, has_crab == "yes")


all_ports$pcid[nrow(all_ports)] <- "coastwide"
all_ports[nrow(all_ports),colnames(coastwide)] <- coastwide

all_ports$average_int[nrow(all_ports)] <- mean(E(cw)$weight)
all_ports$modularity[nrow(all_ports)] <- length(cluster_walktrap(cw))
all_ports$connectance[nrow(all_ports)] <- sum(E(cw)$weight)/length(V(cw))
all_ports$pot_1centrality[nrow(all_ports)] <- closeness(cw,vids="dungeness crab\ncrab pot")

write.csv(all_ports, "Analysis/ses_network/figures/participation_plots/network_stats.csv")
