# plot composition of each fishery
require(ggplot2); require(ggthemes); require(reshape2)

plot_centroid <- function(cluster_sol=cluster_sol, prop_table=prop_table, dat_setup=dat_setup){
  
  meds <- dat_setup[cluster_sol$i.med,"row"]
  to_plot <- prop_table[meds,2:ncol(prop_table)]
  to_plot$cluster <- 1:nrow(to_plot)
  
  one_df <- melt(to_plot,id="cluster")
  one_df <- one_df[one_df$value>0,]
  one_df <- one_df[order(one_df$cluster),]
  colnames(one_df) <- c("cluster","species","catch_proportion")
  
  barplot(one_df$catch_proportion, names.arg = paste("Cluster",one_df$cluster,sep=" "),bor=F, ylab="Proportion catch", xlab="Cluster mediod")
  spacing <- seq(.7, by=1.2, length=8)
  text(spacing, .5,one_df$species)
}

plot_medians <- function(cluster_ind, prop_table){
  cluster <- as.vector(cluster_ind[,"cluster"])
  pt <- as.data.frame(prop_table[,2:ncol(prop_table)])
  
  medians <- aggregate(pt, cluster, median)
  sds <- aggregate(pt, cluster, sd)
  
  barplot(as.matrix(medians[,-1]),beside=T)
  
  boxplot()
}

# pie chart
plot_cp <- function(target_mgmt, cp, prop_table){
    require(RColorBrewer)
    #prop_table$cluster <- cluster_ind[,2]
    
    bar <- subset(prop_table, c8==cp, select=c(-c8, -ftid))
    column <- which(names(bar)==target_mgmt)
    
    grnd <- length(which(bar[,column]==1))
    
    grnd_other <- subset(bar, bar[,column]!=1)    
    
    par(mfrow=c(3,5))
    
    paint <- brewer.pal(9, "YlGnBu")
    paint <- paint[2:9]
    paint <- colorRampPalette(paint)(14)
    
    grnd_trip <- c(grnd/nrow(bar), 1-grnd/nrow(bar))
    names(grnd_trip) <- c(paste("trips\ncomposed\nof 100%\n",target_mgmt,sep=""), paste("trips\nwith <\n100%\n",target_mgmt,sep=""))
    pie(grnd_trip,border=NA, col=c(paint[7],paint[3]), main=paste(nrow(bar), " trips total",sep=""))
    text(-.2, .3, paste(grnd,"trips"), col=paint[1],cex=1.15)
    text(.25, -.25, paste(nrow(bar)-grnd, "trips"),col=paint[1], cex=1.15)
    
    # for each other species, where the trips were non-zero, what proportion of catch did they represent?
    for(i in 1:ncol(grnd_other)){
    
      if(length(grnd_other[,i][grnd_other[,i]>0])==0) {plot(0,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',main=paste("0 ",names(grnd_other[i])," trips",sep=""))}else
      {hist(grnd_other[,i][grnd_other[,i]>0],
         main=paste(length(grnd_other[,i][grnd_other[,i]>0])," non-zero ",names(grnd_other[i]), " trips",sep=""),
         xlab="proportion of catch", col=paint[i], bor=F, 
         sub=paste("[",round(length(grnd_other[,i][grnd_other[,i]>0])/nrow(grnd_other),digits=2)*100, "% of trips]",sep=""))
    }}
}

