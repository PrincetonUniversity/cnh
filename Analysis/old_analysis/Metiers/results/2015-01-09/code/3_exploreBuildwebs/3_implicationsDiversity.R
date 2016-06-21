# are strongly linked fisheries negatively correlated in catch volumes?
#----
# how to define "strongly linked"?
  # in 75th percentile of edge strengths within that network?
# plot distribution of edge strengths, take anything in top top quantile and examine these fisheries

# plot distribution of edge strengths
  hist(E(compare_ports[["Newport"]])$weight)
# function to calculate correlation between time series of revenue and landings
  top_quantile <- function(g){
    return(quantile(E(g)$weight)[4])
    } # returns value above which edge widths need to be. 

# estimate correlation in landings between them over time
  pair_cor <- function(pairs){ 
    # pairs should be 2 col dataframe with pairs of fisheries to consider
    # go through each pair to calculate correlation
    pairs$rev_cor <- NA
    pairs$rev_sig <- NA
    pairs$land_cor <- NA
    pairs$land_sig <- NA
    
    for(i in 1:nrow(pairs)){
      fishA <- subset(tickets, metier == pairs[i,1])
      fishB <- subset(tickets, metier==pairs[i,2])
      fa_ts <- ddply(fishA, .(tdate), summarize, 
                     revenue = sum(adj_revenue)/length(unique(trip_id)), 
                     landings = sum(landed_wt)/length(unique(trip_id)))
      fb_ts <- ddply(fishB, .(tdate), summarize, 
                     revenue=sum(adj_revenue)/length(unique(trip_id)),
                     landings = sum(landed_wt)/length(unique(trip_id)))
    
      # make dates properly formated and sort
        fa_ts$tdate <- as.Date(fa_ts$tdate, format = "%d-%b-%y")
        fa_ts <- fa_ts[order(fa_ts$tdate),]
        fb_ts$tdate <- as.Date(fb_ts$tdate, format = "%d-%b-%y")
        fb_ts <- fb_ts[order(fb_ts$tdate),]
    
    # merge time series together
      both <- merge(fa_ts, fb_ts, by="tdate", all=TRUE)
    # make unreported dates = 0
      both$revenue.x[which(is.na(both$revenue.x))] <- 0
      both$revenue.y[which(is.na(both$revenue.y))] <- 0  
      both$landings.x[which(is.na(both$landings.x))] <- 0
      both$landings.y[which(is.na(both$landings.y))] <- 0
  
    pairs$rev_cor[i] <- cor(both$revenue.x, both$revenue.y)
    pairs$rev_sig[i] <- cor.test(both$revenue.x, both$revenue.y)$p.value
    pairs$land_cor[i] <- cor(both$landings.x, both$landings.y)
    pairs$land_sig[i] <- cor.test(both$revenue.x,both$revenue.y)$p.value
    }
    return(pairs)
}

# find pairs of fisheries -- calculate correlation between catch and revenue series
  top_fisheries <- function(g,port){
    # find top quantile
    edges <- E(g)[which(E(g)$weight > top_quantile(g))]
    weights <- E(g)[which(E(g)$weight > top_quantile(g))]$weight
    # find pair of fisheries
    pairs <- as.data.frame(get.edgelist(g)[as.vector(edges),])
    if(ncol(pairs)==1){
      pairs = as.data.frame(t(pairs))
      rownames(pairs) <- NULL
    }
    pairs$weight <- weights
    pairs <- pair_cor(pairs)
    pairs$port <- port 
    return(pairs)
  }
  
  for(i in 1:length(compare_ports)){
    if(i == 1){
      df <- top_fisheries(compare_ports[[i]], names(compare_ports)[i])
    }else{
      new_df <- top_fisheries(compare_ports[[i]],names(compare_ports)[i])
      df <- rbind(df, new_df)
    }
  }
#----
# examining correlation structure between all fisheries by port. 
#----
  library(zoo)
  all <- define_participationPlot()
# time series of catch for 31 fisheries responsible for 95% of the trips
  sub_tickets <- subset(tickets, metier %in% V(all)$name)
  fish_time <- ddply(sub_tickets, .(metier, tdate), summarize, 
                     revenue = sum(adj_revenue)/length(unique(trip_id)), 
                     landings = sum(landed_wt)/length(unique(trip_id)))
  fish_time$tdate <- as.Date(fish_time$tdate, format="%d-%b-%y")
  melt_rev <- melt(fish_time, id.vars = c("metier","tdate"), measure.vars = "revenue")
  cast_rev <- dcast(melt_rev, tdate ~ metier, fun.aggregate = sum)
  cast_rev.ts <- as.zoo(cast_rev[,2:ncol(cast_rev)],order.by =as.Date(cast_rev[,1]))
  rownames(cast_rev) <- cast_rev$tdate
  cast_rev$tdate <- NULL

# plot  
  library(corrplot)
  cor.mtest <- function(mat, conf.level = 0.95) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- lowCI.mat <- uppCI.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    diag(lowCI.mat) <- diag(uppCI.mat) <- 1
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], conf.level = conf.level)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
        lowCI.mat[i, j] <- lowCI.mat[j, i] <- tmp$conf.int[1]
        uppCI.mat[i, j] <- uppCI.mat[j, i] <- tmp$conf.int[2]
      }
    }
    return(list(p.mat, lowCI.mat, uppCI.mat))
  }
  res1 <- cor.mtest(cor(cast_rev.ts),.95)
  
  corrplot(method="color",cor(cast_rev.ts), p.mat=res1[[1]],
           sig.level=0.05,insig="blank",order="hclust",col.axis="black",
           tl.col="slategrey",tl.cex=.5, mar=rep(.25,4))
  
  # find significant, negative correlations
  new_mat <- cor(cast_rev.ts)
  new_mat[which(res1[[1]] > 0.05)] <- NA # remove unsignificant correlations
  new_mat[new_mat>0] <- NA
  new_mat[is.na(new_mat)] <- 0
  g <- graph.adjacency(abs(new_mat), mode="undirected",weighted=TRUE)
  neg_pairs <- as.data.frame(get.edgelist(g))
  
  saveRDS(cast_rev.ts, "/Users/efuller/1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/cast_rev.ts.RDS")
#----
# look through negative pairs
#----
first <- neg_pairs[1,]
#----
  plot(df$max_per, df$land_cor)
  plot(df$min_per, df$land_cor)
  plot(abs(df$max_per - df$min_per), df$land_cor)
  
par(mfrow=c(2,1),mai=rep(.5,4))
  with(both, plot(tdate, revenue.x,type='h',col="tomato",bty="n",  main = paste0("cor: ",round(cor(both$revenue.x, both$revenue.y),2))))
  lines(fb_ts$tdate, fb_ts$revenue, type="h",col="dodgerblue")
  
  with(both, plot(tdate, landings.x, type='h',col="tomato",bty="n", main = paste0("cor: ",round(cor(both$landings.x, both$landings.y),2))))
  lines(fb_ts$tdate, fb_ts$landings, type="h",col="dodgerblue")
  