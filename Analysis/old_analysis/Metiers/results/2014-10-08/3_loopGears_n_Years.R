rm(list=ls())

path <- "/tigress/efuller/classify_trips/2014-10-08/"

gearGroups = c("TLS","TWL","TWS","MSC","POT","HKL","NET")
classYears = c(2009, 2011, 2013)
refYears = c(2010, 2012)

for(i in 1:length(refYears)){
  for(j in 1:length(gearGroups)){
    for(k in 1:length(classYears)){
      system(paste0("Rscript ",path,"2_knn_classify.R ", refYears[i]," ", gearGroups[j], " ",classYears[k] ))
    }
  }
}
