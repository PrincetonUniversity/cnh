library(dplyr)
library(tidyr)

tickets <- readRDS("processedData/catch/1_cleaningData/tickets.RDS")

participation_network <- function(tickets, pcid_choose=NA, year=NA, filter){
  if(!is.na(pcid_choose)){
    tickets = dplyr::filter(tickets, pcid %in% pcid_choose)
  }
  if(!is.na(year)){
    tickets = dplyr::filter(tickets, year %in% year)
  }
  
  n_boats <- tickets %>% filter(drvid!='NONE') %>%
  group_by(year, metier.2010) %>%
  summarize(n_boats = length(unique(drvid))) %>%
  group_by(metier.2010) %>%
  summarize(max_boats = max(n_boats))


  boats <- tickets %>% filter(drvid != 'NONE') %>%
    mutate(tdate = as.Date(tdate, format = "%d-%b-%y"), 
           doy = as.numeric(format(tdate, '%j')), 
           crab_year = ifelse(doy<305, year-1, year)) %>%
    group_by(drvid, metier.2010, year) %>%
    summarize(revenue = sum(adj_revenue)) %>%
    spread(metier.2010, revenue, fill = NA)
  boats <- as.data.frame(boats)
  rownames(boats) <- paste(boats$drvid, boats$year, sep="_")
  boats$drvid <- NULL
  boats$year <- NULL
  # remove the one boat that didn't sell catch (i.e. rev = 0)
  if(any(rowSums(boats,na.rm=T)==0)){boats <- boats[-which(rowSums(boats, na.rm=T)==0),]}
  percent_boats <- boats/rowSums(boats, na.rm = T)
  
  # find fisheries where at median contribution is 10%
  percent_contribution = apply(percent_boats, MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  
  # process data: drop 'NONE' drvid, drop metiers if fewer than 3 boats participate
  # in any year, and have to be on average 25% of boats annual revenue
  
  if(filter){
    n_boats = 3
    percent = .25
  }else{
    nb = 0
    percent = 0
  }
  
  fishery_df = as.data.frame(percent_contribution)
  fishery_df$metier.2010 = rownames(fishery_df)
  rownames(fishery_df) <- NULL
  fishery_df <- left_join(fishery_df, n_boats, by = 'metier.2010')
  # build adjacency matrix, where elements are frac rev fishery i * frac rev fishery j * total dollars (sum)
  fisheries <- fishery_df$metier.2010[which(fishery_df$max_boats> nb & fishery_df$percent_contribution>percent)]
  A <- matrix(ncol = length(fisheries), nrow = length(fisheries), data = 0)
  colnames(A) <- fisheries
  rownames(A) <- fisheries
  for(k in 1:nrow(boats)){
    
    for(i in 1:nrow(A)){
      frac_rev_i = percent_boats[k,fisheries[i]]
      if(is.na(frac_rev_i)){next} # if don't fish this, then can skip all other combos
      
      for(j in i:ncol(A)){
        frac_rev_j = percent_boats[k,fisheries[j]]
        if(is.na(frac_rev_j)){next}
        
        total_rev = boats[k, fisheries[i]] + boats[k, fisheries[j]]
        A[i,j] = A[i,j] + frac_rev_i * frac_rev_j * total_rev
      }
    }
    #if(k %% 1000 == 0){cat(paste(' iteration', k))}
  }
  
  # do same for crab_year
  
  library(igraph)
  g <- graph_from_adjacency_matrix(adjmatrix = A, mode = 'undirected', weighted = TRUE, diag= FALSE)
  V(g)$size <- colSums(boats[,fisheries],na.rm=T)
  V(g)$percent_size = apply(percent_boats[,fisheries], MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  V(g)$importance = V(g)$size*V(g)$percent_size
  
  size_df <- cbind(V(g), V(g)$size)
  size_df <- size_df[order(size_df[,2], decreasing = T),]
  size_df <- cbind(size_df, cumsum(size_df[,2])/sum(size_df[,2]))
  
  # drop V which have < 100,000 total revenue over time series
  big_g <- subgraph(g, V(g)[V(g)[rownames(size_df)[which(size_df[,3]<.99)]]])
  return(big_g)
}

# plot coastwide ----
big_g <- participation_network(tickets, filter = TRUE)
l <-layout.fruchterman.reingold(big_g) 
l <- cbind(l, 1:21)
rownames(l) <- V(big_g)$name

# add common names
V(big_g)$common_name = c('sablefish\nlongline', 'black rockfish\nlongline', 
                         'swordfish\nlongline', 'CA halibut\npole', 
                         'sea urchin\ndiving', 'chinook\ngill net',
                         'market squid\npurse seine',
                         'chum salmon\ngill net', 'sockeye\ngill net',
                         'white seabass\ngill net', 'sea cucumber\ndip net',
                         'dungeness\ncrab pot', 'spiny lobster\npot', 
                         'sablefish\npot', 'hagfish\npot', 'chinook\ntroll',
                         'albacore\ntuna troll', 'groundfish\ntrawl', 'CA halibut\ntrawl', 
                         'whiting\nmidwater\ntrawl',
                         'pink shrimp\ntrawl')

# adjust layout manually for national
l[12, 1:2] <- c(0, 0) # pot_1
l[18, 1:2] <- c(.4, .65) # twl_1
l[20, 1:2] <- c(0.2, 1) # twl_7 # whiting
l[21, 1:2] <- c(-.3, .7) # tws_1 # pink shrimp
l[19, 1:2] <- c(.65, .5) # twl_2 # ca halibut trawl
l[16, 1:2] <- c(-0.25, -.5) # tls_1 # chinook troll
l[17, 1:2] <- c(-.5, .15) # tls_2 # albacore troll
l[5, 1:2] <- c(0, -.8) # msc_1
l[14, 1:2] <- c(.5, -1) # pot_4
l[13, 1:2] <- c(-.2, -1.3) #pot_2 # spiny lobster
l[7, 1:2] <- c(-1.6, .1) # net_2 # market squid seine
l[1, 1:2] <- c(.65, -.2) # hkl_1 # sablefish longline
l[2, 1:2] <- c(1, .25) # hkl_2 # black rockfish
l[15, 1:2] <- c(-.85, -.35) # pot_6 # hagfish
l[4, 1:2] <- c(.3, -.5) # hkl_4  # ca halibut pole
l[3, 1:2] <- c(-1.15, -.5) # hkl_23 # swordfish 
l[10, 1:2] <- c(-1, -1.15) # net_8 # white seabass
l[6, 1:2] <- c(-1, .6) # net_1 # chinook gill net
l[8, 1:2] <- c(-1.25, 1) # net_4 # chum salmon gill net
l[9, 1:2] <- c(-1.2, .4) # net_7 # sockey gill net
l[11, 1:2] <- c(.65, -.65) # net_9 # sea cucumber

png('/Users/efuller/Desktop/CNH/Analysis/participation_plots/policy_forum/coastwide.png', width = 1680, height = 947, unit = 'px', res=300)
par(bg='#073642', mai=rep(0,4))
plot(big_g, vertex.size = V(big_g)$size/40000000, vertex.label.cex= .75, 
     vertex.frame.color=NA, layout = l, vertex.label = "", 
     edge.width = sqrt(E(big_g)$weight)/900, edge.color = '#eee8d5',
     vertex.color = '#dc322f', edge.curved = F, axes = F,
     vertex.label.family = 'sans', vertex.label.color = '#b58900')
dev.off()
# find list of ports ----
pcid <- read.csv("Analysis/Metiers/results/2015-01-09/code/data/pcid.csv",
                 stringsAsFactors = FALSE) %>%
  rename(pcid = Pcid)

pcid$other_port <-0
pcid$other_port[grep("OTHER",pcid$Name)] <- 1

# drop other ports

port_rev <- tickets %>% filter(drvid!='NONE') %>% 
  left_join(pcid, by='pcid') %>% 
  filter(other_port==0) %>% group_by(pcid) %>%  
  summarize(revenue=sum(adj_revenue)) %>% ungroup() %>%
  arrange(-revenue)

# do ports responsible for approx 95% of coastwide revenue
port_plots <- port_rev$pcid[1:max(which(cumsum(port_rev$revenue)/sum(port_rev$revenue)<.99))]

# build networks for top ports ----
port_list <- list()
for(i in 1:length(port_plots)){
  port_list[[i]] <- participation_network(tickets, pcid=port_plots[i], 
                                          filter = F)
}

# build dictionary of state versus federal ----
management_level = data.frame(
  metier.2010 = unique(unlist(lapply(port_list, function(x)V(x)$name))),
  stringsAsFactors = FALSE, level = NA)

management_level$level[1] <- 'f'
management_level$level[2] <- 'f'
management_level$level[3] <- 's'
management_level$level[4] <- 's'
management_level$level[5] <- 'f'
management_level$level[6] <- 'f'
management_level$level[7] <- 'f'
management_level$level[8] <- 'f'
management_level$level[9] <- 's'
management_level$level[10] <- 'f'
management_level$level[11] <- 'f'
management_level$level[12] <- 'f'
management_level$level[13] <- 's'
management_level$level[14] <- 'f'
management_level$level[15] <- 'f'
management_level$level[16] <- 's'
management_level$level[17] <- 's'
management_level$level[18] <- 'f'
management_level$level[19] <- 'f'
management_level$level[20] <- 's'
management_level$level[21] <- 's'
management_level$level[22] <- 'f'
management_level$level[23] <- 's'
management_level$level[24] <- 's'
management_level$level[25] <- 's'
management_level$level[26] <- 's'
management_level$level[27] <- 's'
management_level$level[28] <- 'f'
management_level$level[29] <- 's'
management_level$level[30] <- 's'
management_level$level[31] <- 'f'
management_level$level[32] <- 'f'
management_level$level[33] <- 'f'
management_level$level[34] <- 'f'
management_level$level[35] <- 's'
management_level$level[36] <- 'f'
management_level$level[37] <- 'f'
management_level$level[38] <- 'f'
management_level$level[39] <- 'f'
management_level$level[40] <- 'f'
management_level$level[41] <- 'f'
management_level$level[42] <- 's'
management_level$level[43] <- 'f'
management_level$level[44] <- 's'
management_level$level[45] <- 'f'
management_level$level[46] <- 's'
management_level$level[47] <- 'f'
management_level$level[48] <- 's'
management_level$level[49] <- 's'
management_level$level[50] <- 's'
management_level$level[51] <- 's'
management_level$level[52] <- 's'
management_level$level[53] <- 'f'
management_level$level[54] <- 's'
management_level$level[55] <- 's'
management_level$level[56] <- 's'
management_level$level[57] <- 'f'
management_level$level[58] <- 's'
management_level$level[59] <- 'f'
management_level$level[60] <- 'f'
management_level$level[61] <- 's'
management_level$level[62] <- 's'
management_level$level[63] <- 'f'
management_level$level[64] <- 's'

# if federal then color = dodgerblue, else indianred
management_level$paint <- ifelse(management_level$level=='f','dodgerblue','indianred')
port_plots = port_plots[-which(lapply(port_list, function(x) length(V(x)))==0)]
port_list = port_list[-which(lapply(port_list, function(x) length(V(x)))==0)]
# plot ports ----
for(i in 1:length(port_list)){
  V(port_list[[i]])$paint <- (data.frame(metier.2010 = V(port_list[[i]])$name, 
                                         stringsAsFactors = FALSE) %>% 
                                left_join(management_level[,c("metier.2010", "paint")]))$paint
  
  fn = paste0('Analysis/participation_plots/policy_forum/port_networks/',port_plots[i],'.png')
  png(file=fn, width = 5, height = 5, unit = 'in', res = 300)
  par(bg='transparent',mai=rep(0,4))
  plot(port_list[[i]], vertex.size = sqrt(V(port_list[[i]])$size)/300, vertex.label.cex= .75, 
       vertex.frame.color=NA, layout = layout.circle, vertex.label ="", 
       edge.width = sqrt(E(port_list[[i]])$weight)/100, edge.color = 'grey30',
       vertex.color = V(port_list[[i]])$paint, edge.curved = T, axes = F,
       vertex.label.family = 'sans', vertex.label.color = '#b58900') 
  dev.off()
}

# calculating beta eff----
# equation 13 from gao et al
# beta eff = average edge weight + symmetry*heterogeneity
# symmetry = 1 for undirected network
# h = variance in edge weight/average edge weight

beta_eff = function(g){
  beta_eff = edge_density(g) + sqrt(var(E(g)$weight))/edge_density(g)
  return(beta_eff)
}

resilience = data.frame(pcid = port_plots,
           beta_eff = unlist(lapply(port_list, beta_eff)))

resilience$scaled = resilience$beta_eff/max(resilience$beta_eff,na.rm=T)

# plot with spectral for resilience ----
library(maps)
all_ports <- read.csv("processedData/spatial/ports/all_ports.csv",
                      stringsAsFactors = FALSE) %>%
  rename(pcid = Pcid)

resilience <- left_join(resilience, all_ports, by = 'pcid')

library(maps)
library(ggthemes)
states <- map_data("state")
ggplot(resilience, aes(x=lon, y = lat, col = beta_eff, label = pcid))  + geom_point() + geom_text() + coord_map()
resilience$plot_name <- gsub(" ", "\n",resilience$Name)

fn = 'Analysis/participation_plots/policy_forum/port_networks/coastwide.png'
png(file=fn, width = 5, height = 10, unit = 'in', res = 300)
ggplot() + 
  geom_polygon(data = states, aes(x = long, y = lat, group = group), 
               color='#eee8d5', size = .1, fill="#657b83") + 
  geom_point(data = resilience, aes(x = lon, y = lat, color = beta_eff),
             size=1, alpha = .5) +
  geom_text(data = resilience, 
            aes(x = lon, y = lat, label = tolower(Name), color = beta_eff), 
            size = 2, nudge_x=-1) +
  coord_map(xlim =range(resilience$lon,na.rm=T) + c(-5,2),
            ylim = range(resilience$lat, na.rm=T) + c(-.5,.5)) + 
  theme_map() + scale_colour_gradient(low='#fc8d59', high ='#99d594') + 
  theme(panel.background = element_rect(fill = '#073642', colour = '#073642'),
        legend.background = element_rect(fill = "#073642", color = "#073642"),
        legend.text = element_text(color = '#eee8d5'), 
        legend.title=element_text(color='#eee8d5'))
dev.off()

