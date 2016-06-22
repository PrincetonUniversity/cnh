# compare classification results - find agreement between partitions and calculate the adjusted Rand index. 

library(e1071)

agreement <- function (gear, year) {
  df10 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2010p",gear, year,".RDS"))
  df12 <- readRDS(paste0("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/classify/2012p",gear, year,".RDS"))
  
  df10$predicted_metier <- paste0(df10$predicted_metier,"_10")
  df12$predicted_metier <- paste0(df12$predicted_metier,"_12")
  
  predicted_df <- merge(df10, df12, by = "trip_id")
  table(predicted_df$predicted_metier.x, predicted_df$predicted_metier.y)
}

# modified rand index, all above 90%. except for NET 2013. :(
classAgreement(agreement("HKL",2013))$crand
classAgreement(agreement("TWS",2013))$crand


# grgroups <- c("TLS","TWS","TWL","POT","NET","HKL","MSC")
grgroups <- c("TLS","TWS","TWL","NET","HKL","MSC")
years <- c(2009, 2011, 2013)

ARI <- matrix(NA, ncol = length(years), nrow = length(grgroups))
rownames(ARI) <- grgroups
colnames(ARI) <- years

for(i in 1:length(grgroups)){
  for(j in 1:length(years)){
    ARI[i,j] <- classAgreement(agreement(grgroups[i],years[j]))$crand
  }
}

POT <- c(NA,classAgreement(agreement("POT",2011))$crand,classAgreement(agreement("POT",2013))$crand)
ARI <- rbind(ARI, POT)
saveRDS(ARI, "/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-20/analyze_metiers/adjustRandIndex.RDS")
