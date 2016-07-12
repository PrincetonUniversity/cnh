participation_network <- function(tickets, pcid_choose=NA, year_choose=NA, filter){
  if(!is.na(pcid_choose)){
    tickets = dplyr::filter(tickets, pcgroup %in% pcid_choose)
  }
  if(any(!is.na(year_choose))){
    tickets = dplyr::filter(tickets, year %in% year_choose)
  }
  
  if(nrow(tickets)==0){
    return(NA)
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
    nb = 3
    percent = .25
  }else{
    nb = 0
    percent = 0
  }
  
  fishery_df = as.data.frame(percent_contribution)
  fishery_df$metier.2010 = rownames(fishery_df)
  rownames(fishery_df) <- NULL
  fish_df <- left_join(fishery_df, n_boats, by = 'metier.2010')
  # build adjacency matrix, where elements are frac rev fishery i * frac rev fishery j * total dollars (sum)
  fisheries <- fish_df$metier.2010[which(fish_df$max_boats> nb & 
                                           fish_df$percent_contribution>percent)]
  if(length(fisheries)==0){
    return(NA)
  }
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
  
  g <- graph_from_adjacency_matrix(adjmatrix = A, mode = 'undirected', weighted = TRUE, diag= FALSE)
  vertex_size = sum(boats[,fisheries], na.rm=T)
  V(g)$size <- vertex_size
  V(g)$percent_size = apply(percent_boats[,fisheries, drop=FALSE], MARGIN = 2, FUN = function(x) median(x, na.rm=T))
  V(g)$importance = V(g)$size*V(g)$percent_size
  
  # if filter = TRUE, keep V which make up to 99% of revenue
  if(filter){
    if(length(V(g))==1){
      big_g <- g
    }else{
      # calculate % cumulative revenue fisheries are responsible for
      size_df <- cbind(V(g), V(g)$size)
      size_df <- size_df[order(size_df[,2], decreasing = T),]
      size_df <- cbind(size_df, cumsum(size_df[,2])/sum(size_df[,2]))
      
      big_g <- induced_subgraph(g, V(g)[V(g)[rownames(size_df)[which(size_df[,3]<.99)]]])
    }
  }else{
    big_g <- g
  }
  return(big_g)
}
