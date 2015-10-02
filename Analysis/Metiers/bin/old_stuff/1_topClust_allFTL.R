# This script sorts the catch data into catch per management group (as delineated in PacFin's database) and clusters
# input: fish tickets (FTL) from PacFin and spid which has been pulled from PacFin website (http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt)
# output: clustdatprop.Rdata - cluster solutions from 1 - max.clusters; clust_prop is the 8 cluster solution along with information; tickets.Rdata - tickets with selected columns plus cp8 appended; prop_table.Rdata the by trip proportion of each mgmt group 

require(plyr); require(dplyr); require(data.table); require(reshape2); require(ggplot2); require(qgraph); require(RColorBrewer); require(cluster)

CNH <- "/Volumes/NOAA_Data/CNH/"

# load fish ticket data
  FTL <- read.csv(paste(CNH,"Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv",sep=""), stringsAsFactors=F, skip=2)
  # drop last two rows, database stuff
  FTL <- head(FTL,-2)

# load species ID data
  spid <- read.csv(paste(CNH,"Analysis/Metiers/data/spid.csv",sep=""),stringsAsFactors=F)

# what are the different management groups?
  mgmt_grp <- dlply(spid, .(mgmt_grp))
  # remove the NA ones
  mgmt_grp <- mgmt_grp[2:9]

# additional column for management group
  m.vec <- rep(NA,nrow(FTL))
  for(i in 1:length(mgmt_grp)){
    m.vec[FTL$spid %in% mgmt_grp[[i]]$SPID] = names(mgmt_grp[i])
  }

# take only info need from FTL
  tickets <- select(FTL, ftid, veid, year, spid, landed_wt, ppp, grgroup, grid, tdate, pcid, ifq_landing, processorid, pargrp)
  tickets$mgmt_grp <- m.vec
  # remove FTL from working memory
  rm(FTL)

# now look at amount caught per trip
  by_trip <- data.table(tickets)
  setkey(by_trip, ftid)
  catch <- by_trip[, sum(landed_wt), by=c("ftid","mgmt_grp")]
  total_catch <- dcast.data.table(catch, ftid ~ mgmt_grp, fun=sum)

  # find proportion table
    prop_table <- as.data.frame(total_catch)
    prop_table[,2:ncol(prop_table)] <- prop_table[,2:ncol(prop_table)]/rowSums(prop_table[,2:ncol(prop_table)])

# do pca
  pca_prop <- prcomp(prop_table[,2:ncol(prop_table)],scale=T)
  npc <- length(which(summary(pca_prop)[[6]][3,]<= .81)) # is 5

  # retain principal components which retain > 80% of variation
    dat_setup <- pca_prop$x[,1:npc]

# prepare for clustering
  # randomize rows to prevent any bias from order of fish tickets
  # set up row index, so can translate back to original order
  row <- seq(1:nrow(dat_setup))
  dat_setup <- cbind(dat_setup,row)
  dat_setup <- dat_setup[sample(nrow(dat_setup)),]
  dat_prop <- dat_setup[,1:(ncol(dat_setup)-1)] # data minus row number for clustering

  # set cluster info/data structures
    max.clusts = 30
    samples = 100
    sampsize = 1000
    clust.dat.prop <- vector("list",length = max.clusts)

# clustering
  set.seed(2)
  for(i in 1:max.clusts){
    clust.dat.prop[[i]] <- clara(dat_prop, i, stand=TRUE, samples = samples, sampsize=sampsize, keep.data = FALSE, pamLike=TRUE, rngR = TRUE)
    cat(i,"...")
  }
  # save results
  save(clust.dat.prop, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/clustdatprop_",Sys.Date(),".Rdata",sep=""))

# evaluate clustering
  objectives <- vector(length = max.clusts)
  for (i in 1:max.clusts) { objectives[i] <- clust.dat.prop[[i]]$objective }

  asw <- vector(length = max.clusts)
  for (i in 2:max.clusts) { asw[i] <- clust.dat.prop[[i]]$silinfo$avg.width  }
    
  par(mfrow=c(1,2),omi=c(0,0,0,0))
  plot(objectives, type="o",pch=19, cex=0.35,bty="n", main="Objective function",col="tomato1")
  points(8,objectives[8],lwd=5, col="tomato4")
  plot(asw, type="o", pch=19, cex=0.35, bty="n", main="Silhouette Width",col="turquoise")
  points(8,asw[8],lwd=5,col="turquoise4")

# 8 groups looks like the best fit -- see Mmgt_cluster_writeup.Rmd for more details
  # save solution with documentation
  nodes = 8
  clust_prop <- list(data_transform="prop",cluster_sol = nodes, objectives = objectives, asw = asw, clustering = clust.dat.prop[[nodes]]$clustering, sampsize=sampsize, samples=samples, npc=npc)
   
  # map clustering to data
  ind <- cbind(clust_prop$clustering,dat_setup[,"row"])

  prop_table$c8[ind[,2]] <- ind[,1]

  mc <- select(prop_table,ftid,c8)
  tickets <- merge(tickets, mc, by="ftid")

# save tickets for later use
save(tickets, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets_",Sys.Date(),".Rdata",sep=""))
save(prop_table, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/prop_table_",Sys.Date(),".Rdata",sep=""))

# save cluster solution
save(clust_prop, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/clust_prop_",Sys.Date(),".Rdata",sep=""))
