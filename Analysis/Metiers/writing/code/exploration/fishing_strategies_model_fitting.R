# fitting a model to predict cv_adj_revenue based on cluster id and diversity
yrdf <- readRDS("/Users/efuller/1/CNH/Analysis/Metiers/writing/code/1_cleaningData/yrdf.RDS")

lm_reg <- lm(cv_adj_revenue ~ mean_simpson + I(mean_simpson^2), data = yrdf[[2]])
lm_pp <- lm(cv_adj_revenue ~ mean_simpson + I(mean_simpson^2) + factor(single_cluster), data = yrdf[[2]])

library(mgcv)
gam_pp <- gam(cv_adj_revenue ~ mean_simpson + single_cluster, data=yrdf[[2]])

AIC(lm_reg)-AIC(lm_pp)

# look at what metiers are in each profile. but really want to know about distribution of revenue across these. below doesn't do that yet. 
profile_trips <- subset(data, cluster == 8)
profile_trips <- unique(profile_trips[,c("drvid","year","metier","trip_id")])
# composition
comp <- sort(table(profile_trips$metier),decreasing=T)/sum(table(profile_trips$metier))
barplot(comp[which(comp>.1)])

yrdf[[2]]$fsingle_cluster <- factor(yrdf[[2]]$single_cluster)
bymedian <- with(yrdf[[2]], reorder(fsingle_cluster, cv_adj_revenue, median))
boxplot(cv_adj_revenue ~ bymedian, data = yrdf[[2]], col="grey")