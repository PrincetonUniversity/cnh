# make some directed graphs
library(igraph); library(reshape2); library(dplyr); library(vegan)

# load data ----
filtered_ftl <- readRDS("/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/data_output/diversity_landings_data.RDS")

# common names and colors ----
descrp <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/catch/3_exploreBuildwebs/ref_tables/metier_descrp.csv", stringsAsFactors = FALSE)

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

# define participation plots ----
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
  cn <- cn[match(tolower(V(g)$name), cn$metier),] # reorder to match
  
  g_s <- g
  V(g_s)$name <- cn$cn
  
  V(g_s)$color <- cn$paint
  
  l <- layout.fruchterman.reingold(g_s,niter=500)
  
  #plot(g_s, edge.width = E(g_s)$weight*3, layout = l, vertex.size = log(V(g_s)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
  
  return(g_s)
}

# take each port in tickets ----
port_list <- list()
prts <- unique(filtered_ftl$pcid)
for(p in 1:length(prts)){
  port_list[[p]] <- define_participationPlot(year_choose = 2009:2010, port = prts[p], restrict = FALSE, tickets = filtered_ftl)
}

names(port_list) <- prts
# remove ports that are NA
port_list <- port_list[-which(is.na(port_list))]

# catch share analysis data ----
ic <- as.data.frame(unlist(lapply(port_list, function(x)sum(E(x)$weight)/vcount(x))))
ic$pcid <- rownames(ic)

port_post <- list()

for(p in 1:length(prts)){
  port_post[[p]] <- define_participationPlot(year_choose = 2012:2013, port = prts[p], restrict = FALSE, tickets = filtered_ftl)
}

names(port_post) <- prts
# remove ports that aren't in port_post
port_post <- port_post[-which(is.na(port_post))]

ic_post <- as.data.frame( unlist(lapply(port_post, function(x)sum(E(x)$weight)/vcount(x))))
ic_post$pcid <- rownames(ic_post)
colnames(ic_post) <- c("ic_post","pcid")

colnames(ic) <- c("ic_pre","pcid")

port_df <- left_join(ic, ic_post)
port_df <- port_df[complete.cases(port_df),]
port_df$ic_delta <- port_df$ic_post - port_df$ic_pre

# mark ifq landings
ifq_landings <- filtered_ftl %>%
  filter(year > 2011) %>%
  group_by(pcid, ifq_landing) %>%
  summarize(revenue = sum(adj_revenue,na.rm = T), 
            lbs = sum(landed_wt, na.rm = T)) 
# count
ifq_present <- table(ifq_landings$pcid, ifq_landings$ifq_landing)

port_df$ifq <- 1
port_df$ifq[which(port_df$pcid %in% rownames(ifq_present)[which(ifq_present[,3]==0)])]  <- 0

# but should remove "other ports, any that start with an O basically"
# merge with port lat dataframe and remove any that don't have a location
ports <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/spatial/ports/all_ports.csv",stringsAsFactors = FALSE)
ports <- rename(ports, pcid = Pcid)
port_df <- left_join(port_df, ports)
port_df <- port_df[-which(is.na(port_df$lat)),]
#port_df[grep("^O",port_df$pcid),] # good, all real

# check to make sure > 3 boats land at each
n_ves <- filtered_ftl %>%
  filter(year > 2011) %>%
  group_by(pcid) %>%
  summarize(nv = length(unique(drvid))) %>%
  filter(nv < 4)

port_df <- subset(port_df, !(pcid %in% n_ves$pcid))

saveRDS(port_df, "/Volumes/LA-PRIVATE/CNH/Analysis/Metiers/bin/data_output/port_diversity_network_metrics.RDS")
