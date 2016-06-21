# build participation networks

# different measures: total is what I'm using now. Could be bray-curtis 
# dissimilarity to look at vessel composition across fisheries or hellinger. 
# A fishery is very similar if the same vessels participate in the same way. 
# Using drvid as vessel ID. measure = "total" is just the intersection/union of 
# vessels that participate in both. 

define_participationPlot <- function(year_choose=2009:2013, port=NA, restrict=TRUE,restrict.level=.95, measure="total",graph=FALSE){
  if(is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose),]}
  if(!is.na(port)){
    yr_tickets <- tickets[which(tickets$year %in% year_choose & tickets$pcid==port),]
    if(nrow(yr_tickets)==0){
      return(NA)
    }
  }
  m_by_v <- melt(yr_tickets, id.vars = c("metier","drvid"), measure.vars = "trip_id")
  m_by_v <- unique(m_by_v)
  if(length(unique(m_by_v$drvid))==1){
    return(NA)
  }
  cast_mv <- dcast(m_by_v, metier~drvid, length)
  rownames(cast_mv) <- cast_mv$metier
  cast_mv <- cast_mv[,-1]
  
  # remove any metiers that aren't used at all
  
  # restrict to metiers responsible for at least 95% of trips 
  if(restrict==TRUE){
    metier_totals <- rowSums(cast_mv)
    total_trips <- sum(metier_totals)
    cumulatives <- cumsum(sort(metier_totals/total_trips,decreasing=T))
    # check to make a single metier not it
    if(cumulatives[1]>restrict.level){
      metiers_to_keep <- names(cumulatives[1])
    }else{
      metiers_to_keep <- names(which(cumulatives<=restrict.level))
    }
    cast_mv <- cast_mv[metiers_to_keep,]
    cast_mv <-cast_mv[,which(colSums(cast_mv)>0),drop=FALSE]
    if(is.integer(cast_mv)){ # if only vessel, data.frame structure disapperas
      cast_mv <- data.frame(vessel_1 = cast_mv)
    }
    m_by_v <- subset(m_by_v, metier %in% metiers_to_keep)
  }
  
  if(measure=="bray_curtis"){
    bc_drvid <- as.matrix(1-vegdist(cast_mv))
  }
  if(measure=="hellinger"){
    bc_drvid <- as.matrix(1-dist(decostand(cast_mv,"hellinger")))
  }
  if(measure=="total"){
    edge.width <- matrix(ncol=nrow(cast_mv), nrow=nrow(cast_mv),0)
    row.names(edge.width) <- row.names(cast_mv)
    colnames(edge.width) <- row.names(cast_mv)
    sizes <- rep(NA, ncol(edge.width))
    for(i in 1:nrow(edge.width)){
      for(j in 1:ncol(edge.width)){
        row_met <- row.names(edge.width)[i]
        col_met <- colnames(edge.width)[j]
        both <- intersect(unique(m_by_v$drvid[which(m_by_v$metier==row_met)]),
                          unique(m_by_v$drvid[which(m_by_v$metier==col_met)]))
        all <- union(unique(m_by_v$drvid[which(m_by_v$metier==row_met)]),
                     unique(m_by_v$drvid[which(m_by_v$metier==col_met)]))
        edge.width[i,j] <- length(both)/length(all)
        if(i==j){sizes[i] <- length(all)}
      }
    }
    bc_drvid <- edge.width
    diag(bc_drvid) <- sizes
    
  }
  
  g <- graph.adjacency(bc_drvid,weighted = TRUE, mode="undirected", diag = FALSE)
  V(g)$size <- diag(bc_drvid)
  
  im_g <- infomap.community(g)
  
  paint <- colorRampPalette(brewer.pal( 8, "Dark2"))(length(im_g))
  V(g)$color <- paint[im_g$membership]
  V(g)$membership <- im_g$membership
  
  if(graph==TRUE){

    
    plot(g,vertex.size=V(g)$size/2, edge.width=E(g)$weight*20, 
         vertex.label.family="sans",layout=layout.spring,
         vertex.frame.color=V(g)$color, vertex.label.color="black")
  }
  return(g)
  
}