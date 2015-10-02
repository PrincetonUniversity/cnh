# analyzing changes in communities before and after catch shares 

landings <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/bin/diversity_landings_data.RDS")

# build participation network given a port and pre/post design
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

# example ports
newport_prior <- define_participationPlot(2009:2010, port = "NEW",tickets = landings, restrict.revenue = TRUE)
saveRDS(newport_prior, "/Users/efuller/1/CNH/Analysis/Metiers/bin/newport_prior.RDS")
ports = unique(landings$pcid)

# running through each port calculating delta average degree
port_df <- data.frame(pcid = unique(landings$pcid))
port_df$mean_degree_prior <- NA; port_df$mean_degree_post <- NA
port_df$mean_betweenness_prior <- NA; port_df$mean_betweenness_post <- NA
port_df$density_prior <- NA; port_df$density_post <- NA

for(i in 1:nrow(port_df)){
  pre <- define_participationPlot(year_choose = 2009:2010, 
                                  port = port_df$pcid[i], 
                                  restrict.revenue = FALSE, 
                                  tickets = landings, 
                                  plot.yes = FALSE)
  post <- define_participationPlot(year_choose = 2012:2013, 
                                   port = port_df$pcid[i], 
                                   restrict.revenue = FALSE, 
                                   tickets = landings, 
                                   plot.yes = FALSE)
  if(any(is.na(pre)) | any(is.na(post))){
    port_df[i,c("mean_degree_prior","mean_degree_post")] <- c(NA,NA)
  }else{
    port_df$mean_degree_prior[i] <- mean(degree(pre))
    port_df$mean_degree_post[i] <- mean(degree(post))
    
    port_df$mean_betweenness_prior[i] <- mean(betweenness(pre))
    port_df$mean_betweenness_post[i] <- mean(betweenness(post))
    
    port_df$density_prior[i] <- edge_density(pre)
    port_df$density_post[i] <- edge_density(post)
  }
}

port_df$delta.degree <- port_df$mean_degree_prior - port_df$mean_degree_post
port_df$delta.density <- port_df$density_prior - port_df$density_post
port_df$delta.mean.betweenness <- port_df$mean_betweenness_prior - port_df$mean_betweenness_post
# label IFQ particpation
ifq_yes <- landings %>%
  filter(year > 2011) %>%
  group_by(pcid) %>%
  summarize(has.ifq = ifelse(any(ifq_landing=="Y"), 1, 0))

port_df <- left_join(port_df, ifq_yes)

saveRDS(port_df, "/Users/efuller/1/CNH/Analysis/Metiers/bin/port_diversity_landings.RDS")
