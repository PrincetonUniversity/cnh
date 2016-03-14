# looking at amount of time each vessel spends within 1.1 km from another vessel

# loading vms ----
window = 24
fp1 <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",
              window,"hr/")
fls <- dir(fp1)
fp <- paste0("processedData/spatial/vms/intermediate/05_make_obs_to_vms/tw_",
             window,"hr/",fls)
vms <- lapply(fp, read.csv)


# debug ncols not same
nc <- sapply(vms, ncol)
# 268 is missing rev_dist, why I have no idea. But add it for now
vms[[268]]$rev_dist <- NA

vms <- do.call(rbind, vms)

# subset to shrimp
shrimp <- subset(vms, metier.2010 == "TWS_1")

# pairwise distance for each vessel (45 of them)
library(sp)
ves_prox <- data.frame(doc.num = unique(shrimp$doc.num),
                       n_hrs_prox = NA, n_hrs_total = NA,
                       pcid = NA,
                       stringsAsFactors = FALSE)

library(sp)
# takes about 12 seconds
system.time({for(i in 1:nrow(ves_prox)){
  v1 <- subset(shrimp, doc.num == ves_prox$doc.num[i])
  v1_mat <- as.matrix(v1[,c("longitude","latitude")])
  vrest <- subset(shrimp, doc.num != ves_prox$doc.num[i])
  vrest_mat <- as.matrix(vrest[,c('longitude','latitude')])
  ds <- spDistsN1(vrest_mat, v1_mat[1,], longlat = TRUE)
  ves_prox$n_hrs_prox[i] <- length(which(ds<= 1.1))
  ves_prox$n_hrs_total[i] <- length(ds)
  if(any(!is.na(v1$pcid))){
    ves_prox$pcid[i] <- names(which.max(table(v1$pcid)))
    }
}})

# one vessel takes about 4 min. 
system.time({for(i in 1:nrow(ves_prox)){
  v1 <- subset(shrimp, doc.num == ves_prox$doc.num[i])
  v1_mat <- as.matrix(v1[,c("longitude","latitude")])
  vrest <- subset(shrimp, doc.num != ves_prox$doc.num[i])
  vrest_mat <- as.matrix(vrest[,c('longitude','latitude')])
  d_close <- rep(NA,nrow(v1_mat))
  system.time({for(g in 5889:nrow(v1_mat)){
    ds <- spDistsN1(vrest_mat, v1_mat[g,], longlat = TRUE)
    d_close[g] <- ifelse(any(ds<= 1.1), 1, 0)
  }})
  ves_prox$n_hrs_prox[i] <- sum(d_close)
  ves_prox$n_hrs_total[i] <- length(d_close)
  if(any(!is.na(v1$pcid))){
    ves_prox$pcid[i] <- names(which.max(table(v1$pcid)))
  }
}})

ves_prox$prop_hrs <- ves_prox$n_hrs_prox/ves_prox$n_hrs_total

# subset to just astoria and coos bay with more sample size
with(subset(ves_prox, pcid %in% c("AST" ,"COS")), 
     boxplot(n_hrs_prox/n_hrs_total ~ pcid))

library(ggplot2)
ggplot(subset(ves_prox, pcid %in% c("AST","COS")), 
       aes(x = pcid, y =prop_hrs))+ geom_boxplot() 

ggplot(subset(ves_prox, pcid %in% c("AST","COS")),
       aes(x = prop_hrs, group = pcid)) + 
  geom_histogram(position = 'dodge',aes(fill=pcid), bins = 15)

library(ggthemes)
library(dplyr)
DF <- ves_prox %>% group_by(pcid) %>% mutate(n_boats = n())
ggplot(DF, aes(x = reorder(pcid, prop_hrs, median), y = prop_hrs)) + 
  geom_boxplot(aes(fill = n_boats), color='grey50')  + theme_pander()

ggplot(ves_prox, aes(x = prop_hrs)) + geom_histogram(bins = 10) + theme_pander()

# not strong. but perhaps with behavioral segmentation
# get home port for each vessel
