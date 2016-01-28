# This script sorts the catch data into catch per management group (as delineated in PacFin's database) and clusters
# input: fish tickets (FTL) from PacFin and spid which has been pulled from PacFin website (http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/sp.txt)
# output: clustdatprop.Rdata - cluster solutions from 1 - max.clusters; clust_prop is the 8 cluster solution along with information; tickets.Rdata - tickets with selected columns plus cp8 appended; prop_table.Rdata the by trip proportion of each mgmt group 
rm(list=ls())
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

# add some nuance, for OTHR, split up complexes
complex <- dlply(mgmt_grp[["OTHR"]], .(complex))
complex2 <- dlply(mgmt_grp[["OTHR"]], .(complex2))

mgmt_grp[["SHRK"]] <- complex2[["SHRK"]]
mgmt_grp[["SKAT"]] <- complex2[["SKAT"]]
mgmt_grp[["BASS"]] <- complex[["BASS"]]
mgmt_grp[["ECHN"]] <- complex[["ECHN"]]
mgmt_grp[["MLSK"]] <- complex[["MLSK"]]
mgmt_grp[["STRG"]] <- complex[["STRG"]]
mgmt_grp[["TUNA"]] <- complex[["TUNA"]]

new_other <- intersect(complex[["NA"]],complex2[["NA"]])

nrow(mgmt_grp[["SHRK"]]) + nrow(mgmt_grp[["SKAT"]]) + nrow(mgmt_grp[["BASS"]]) + nrow(mgmt_grp[["ECHN"]]) + nrow(mgmt_grp[["MLSK"]]) + nrow(mgmt_grp[["STRG"]]) + nrow(mgmt_grp[["TUNA"]]) + nrow(new_other) == nrow(mgmt_grp[["OTHR"]]) # good didn't loose any. 

mgmt_grp[["OTHR"]] <- new_other



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

# alternate, what about a combination of complexes and management groups
code_spid <- subset(spid, X==1)
code_spid$code <- rep(0,nrow(code_spid))
for(i in 1:nrow(code_spid)){
  code_spid$code[i] <-paste(code_spid$complex[i], code_spid$mgmt_grp[i], code_spid$complex2[i], collapse="")
}

tickets <- merge(tickets, code_spid[,c("SPID","code")],by.x="spid", by.y="SPID",all.x=TRUE, all.y=FALSE)


# now look at amount caught per trip
  by_trip <- data.table(tickets)
  setkey(by_trip, ftid)
  catch <- by_trip[, sum(landed_wt), by=c("ftid","code")]
  total_catch <- dcast.data.table(catch, ftid ~ code, fun=sum)

  # find proportion table
    prop_table <- as.data.frame(total_catch)
    prop_table[,2:ncol(prop_table)] <- prop_table[,2:ncol(prop_table)]/rowSums(prop_table[,2:ncol(prop_table)])

# do pca
  pca_prop <- prcomp(prop_table[,2:ncol(prop_table)],scale=T)
  npc <- length(which(summary(pca_prop)[[6]][3,]<= .81)) # is 10

  # retain principal components which retain > 80% of variation
    dat_setup <- pca_prop$x[,1:npc]

# prepare for clustering
  # randomize rows to prevent any bias from order of fish tickets
  # set up row index, so can translate back to original order
  row <- seq(1:nrow(dat_setup))
  dat_setup <- cbind(dat_setup,row)
  dat_setup <- dat_setup[sample(nrow(dat_setup)),]
  dat_prop <- dat_setup[,1:(ncol(dat_setup)-1)] # data minus row number for clustering

  # save dat_setup for referenc
    save(dat_setup, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/dat_setup_",Sys.Date(),".Rdata",sep=""))
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
  points(10,objectives[10],lwd=5, col="tomato4")
  plot(asw, type="o", pch=19, cex=0.35, bty="n", main="Silhouette Width",col="turquoise")
  points(10,asw[10],lwd=5,col="turquoise4")

# 10 groups looks like the best fit -- see Mmgt_cluster_writeup.Rmd for more details
  # save solution with documentation
  nodes = 10
  clust_prop10 <- list(data_transform="prop",cluster_sol = nodes, objectives = objectives, asw = asw, clustering = clust.dat.prop[[nodes]]$clustering, sampsize=sampsize, samples=samples, npc=npc, mediods = clust.dat.prop[[nodes]]$i.med)
clust_prop10[["mediods"]] <- prop_table[ind[,2][clust_prop10[["mediods"]]],] # table of mediods
   
  # map clustering to data
  ind <- cbind(clust_prop10$clustering,dat_setup[,"row"])

  prop_table$c8[ind[,2]] <- ind[,1]

  mc <- select(prop_table,ftid,c8)
  tickets <- merge(tickets, mc, by="ftid")

# save tickets for later use
save(tickets, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/tickets_",Sys.Date(),".Rdata",sep=""))
save(prop_table, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/prop_table_",Sys.Date(),".Rdata",sep=""))

# save cluster solution
save(clust_prop10, file=paste("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/clust_prop_",Sys.Date(),".Rdata",sep=""))
