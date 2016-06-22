# guts of LDA run. Parameters and loading data
ks = commandArgs(trailingOnly = TRUE)
burnin <- 1000
iter <- 1000 
keep <- 50 

readRDS("/tigress/efuller/fish_topics/trip_mat.RDS")

model <- lapply(ks, function(k) LDA(triplet, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep, verbose = 5)))

saveRDS(model, file = paste("/tigress/efuller/fish_topics/model_",ks,".RDS",sep=""))
