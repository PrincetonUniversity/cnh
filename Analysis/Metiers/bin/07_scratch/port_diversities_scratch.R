# make some directed graphs
library(igraph); library(reshape2); library(dplyr); library(vegan)
# load data ----
dat <- readRDS("/Volumes/LA-PRIVATE/CNH/processedData/catch/1_cleaningData/tickets.RDS")

# vessels should have less than min_rev on average across 5 years AND
# landings before and after catch shares
# then drop 2011 data
# also drop any ZZZ drvids

min_rev = 5000

library(dplyr)

div_dat <- dat[-grep("ZZZ",dat$drvid)] %>%                    # drop ZZZ vessels
  group_by(drvid, year) %>%                                   
  summarize(annual_revenue = sum(adj_revenue, na.rm = T)) %>% # calculate yr rev
  mutate(av.annual.rev = mean(annual_revenue, na.rm = T)) %>%     # mean yr rev
  filter(av.annual.rev >= min_rev) %>%           # drop vessels with < min_rev
  filter(year != 2011) %>%
  mutate(post.itq = ifelse(year>2011, 1, 0)) %>%  # mark before and after itqs
  group_by(drvid) %>%
  mutate(both = ifelse(length(unique(post.itq))==2, 1, 0)) %>% # mark ves present both
  filter(both == 1)

filtered_ftl <- subset(dat, drvid %in% unique(div_dat$drvid))

library(igraph); library(reshape2); library(vegan)

# calculate the bray curtis dissimilarity to look at vessel composition across fisheries. A fishery is very similar if the same vessels participate in the same way. 
# need a metier by drvid matrix to run vegdist on

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

# hm, the color ramps dont' look great. maybe should just do a single color for each gear group

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
  
  plot(g_s, edge.width = E(g_s)$weight*3, layout = l, vertex.size = log(V(g_s)$size)*4, vertex.label.color = "grey30", vertex.label.cex = .75, vertex.label.family="sans", vertex.frame.color = NA, edge.curved=TRUE, edge.arrow.size = .25, main = port, edge.color = "black")
  
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

# make some plots ----
# doesn't look great - think the directed links inflate tiny networks
barplot(sort(unlist(lapply(port_list, graph.density)),decreasing = TRUE),las=2, cex.names=.75)

# looks a bit better
barplot(sort(unlist(lapply(port_list, function(x)ecount(x)/vcount(x))),decreasing = TRUE),las=2, cex.names=.75)

# make inverse list
make_cost <- function(graph){
  inv_port <- graph
  E(inv_port)$weight <- 1/E(graph)$weight
  return(mean_distance(inv_port))
}

# again, really small networks get low mean distances because few nodes
barplot(sort(unlist(lapply(port_list, make_cost)),decreasing = TRUE),las=2, cex.names=.75)

# This is crazy, how can CRS have such a high median betweenness?
barplot(sort(unlist(lapply(port_list, function(x)median(betweenness(x)))),decreasing = TRUE),las=2, cex.names=.75)

barplot(sort(unlist(lapply(port_list, function(x)mean(edge_betweenness(x)))),decreasing = TRUE),las=2, cex.names=.75)

# also crazy looking. conclude that i don't know what betweenness is measuring
barplot(sort(unlist(lapply(port_list, function(x)median(edge_betweenness(x)))),decreasing = TRUE),las=2, cex.names=.75)

# possible that betweenness also uses weights as costs, so do inverse for that too
inv_betwn <- function(graph){
  inv_port <- graph
  E(inv_port)$weight <- 1/E(graph)$weight
  return(betweenness(inv_port))
}

# looks a bit better. but maybe divide by total number of nodes to get a feel for relative to network size
barplot(sort(unlist(lapply(port_list, function(x)mean(inv_betwn(x)))),decreasing = TRUE),las=2, cex.names=.75)

barplot(sort(unlist(lapply(port_list, function(x)mean(inv_betwn(x))/vcount(x))),decreasing = TRUE),las=2, cex.names=.75) # still ORF is low, confused. 

# what about looking at diameter -- i bet it uses weights as distances so inverse
inv_diameter <- function(graph){
  inv_port <- graph
  E(inv_port)$weight <- 1/E(graph)$weight
  return(diameter(inv_port))
}
barplot(sort(unlist(lapply(port_list, inv_diameter)), decreasing = TRUE),las =2, cex.names=.5)

# v count over diameter
barplot(sort(unlist(lapply(port_list, function(x)vcount(x)/inv_diameter(x))), decreasing = TRUE))

# not great, still not sure I like diameter.

# what about just looking at the sum of the edge weights: ends up prioritizing big networks with lots of connections
barplot(sort(unlist(lapply(port_list, function(x)sum(E(x)$weight))), decreasing = TRUE),las =2, cex.names=.5)

# could also look at normalized by number of nodes
barplot(sort(unlist(lapply(port_list, function(x)sum(E(x)$weight)/vcount(x))), decreasing = TRUE),las =2, cex.names=.5)

# like both of these. what does the value mean though. it's the node average weight. and knowing that connections can't be more than 1, you have networks where there are around 6 fisheries connected strongly, or if interconnectedness = 1, then three fisheries are connected at 33%, 

# look at the two against one another
my_met <- unlist(lapply(port_list, function(x)sum(E(x)$weight)/vcount(x)))
bets <- unlist(lapply(port_list, function(x)mean(inv_betwn(x))))
plot(my_met, bets)

# catch share analysis ----
ic <- as.data.frame(unlist(lapply(port_list, function(x)sum(E(x)$weight)/vcount(x))))
ic$pcid <- rownames(ic)

port_post <- list()

for(p in 1:length(prts)){
  port_post[[p]] <- define_participationPlot(year_choose = 2012:2013, port = prts[p], restrict = FALSE, tickets = filtered_ftl)
}

names(port_post) <- prts
# remove ports that aren't in port_pre


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
  
  port_lm <- lm(ic_delta ~ ifq, port_df)
  port_lm2 <- lm(ic_delta ~ ic_pre + ifq, port_df)
  # so now a very minorly significant result 
  
  # but should remove "other ports, any that start with an O basically"
  # merge with port lat dataframe and remove any that don't have a location
  ports <- read.csv("/Volumes/LA-PRIVATE/CNH/processedData/spatial/ports/all_ports.csv",
                    stringsAsFactors = FALSE)
  ports <- rename(ports, pcid = Pcid)
  port_df <- left_join(port_df, ports)
  port_df <- port_df[-which(is.na(port_df$lat)),]
  port_df[grep("^O",port_df$pcid),] # good, all real
  
  port_lm <- lm(ic_delta ~ ifq, port_df)
  port_lm2 <- lm(ic_delta ~ ic_pre + ifq, port_df)
# nope, on edge of being significant. but not. phew. but close
  # should simulate and make new figures. 
  # then do analysis of ranges, 
  
  
# scratch ----
crs <- define_participationPlot(year_choose = 2009:2010, port = "CRS", restrict = FALSE, tickets = filtered_ftl)
graph.density(crs)
hist(betweenness(crs))

sb <- define_participationPlot(year_choose = 2009:2010, port = "SB", restrict = FALSE, tickets = filtered_ftl)
graph.density(sb)
hist(betweenness(sb))
plot(density(betweenness(sb)),col='red')
lines(density(betweenness(crs)), col = 'yellow')

plot(density(E(sb)$weight),col='red')
lines(density(E(crs)$weight), col = 'yellow')

mro <- define_participationPlot(year_choose = 2009:2010, port= "MRO", restrict = FALSE, tickets = filtered_ftl)

tll <- define_participationPlot(year_choose = 2009:2010, port = "TLL", restrict = FALSE, tickets = filtered_ftl)
ecount(tll)/vcount(tll)
ecount(crs)/vcount(crs)



# what about shortest paths
# have to take inverse of weights/because shortest path thinks of them as distances/costs
inv_sb <- sb; E(inv_sb)$weight <- 1/E(sb)$weight
mean_distance(inv_sb)

inv_crs <- crs; E(inv_crs)$weight <- 1/E(crs)$weight
mean_distance(inv_crs)

inv_tll <- tll; E(inv_tll)$weight <- 1/E(tll)$weight
mean_distance(inv_tll)

inv_mro <- mro; E(inv_mro)$weight <- 1/E(mro)$weight
mean_distance(inv_mro)

hist(graph.diversity(crs))
foo <- round(get.adjacency(g_s, attr = "weight"),digits = 2)
tabasco(as.matrix(foo))


betweenness(g_s)


newport <- define_participationPlot(2009:2013, "NEW", tickets = filtered_ftl)

paint_df <- data.frame(as.character(V(newport)$name),stringsAsFactors = FALSE)
