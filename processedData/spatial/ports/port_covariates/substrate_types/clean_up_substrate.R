substrate_all <- read.csv("/Users/efuller/1/CNH/processedData/spatial/ports/port_covariates/substrate_types/substrate_all.csv",stringsAsFactors = FALSE)
substrate_all$X <- NULL
colnames(substrate_all) <- c("pcid","no.data.m2", "s1.m2","s2.m2","s3.m2","s4.m2","per.no.data","per.s1","per.s2","per.s3","per.s4")

# drop percent columns - will recalculate
substrate_all <- substrate_all[,-grep("per",colnames(substrate_all))]

# flag ports with less than 5e7 m2 in area - suggests low coverage of habitat layer
# i.e.
total_areas <- apply(substrate_all[,2:6], sum, MARGIN = 1)
hist(total_areas,col="grey",bor=F)
abline(v=5e7,col="indianred",lwd=5)

substrate_all$poor_coverage <- ifelse(total_areas<5e7, 1, 0)

# calculate percents for each coverage
# calculate covered area
substrate_all$total_habitat.m2 <- apply(substrate_all[,3:6], sum, MARGIN = 1)
substrate_all$per.s1 <- 

# after further reflection, probably better measure is total measured habitat
  hist(substrate_all$total_habitat.m2,breaks=30)
# so maybe mark ports with measured habitat < 1e7. 

substrate_all$poor_coverage <- ifelse(substrate_all$total_habitat.m2<1e7, 1, 0)

# calculate percent of each habitat type
substrate_all$per.s1 <- (substrate_all$s1.m2/substrate_all$total_habitat.m2)*100
substrate_all$per.s2 <- (substrate_all$s2.m2/substrate_all$total_habitat.m2)*100
substrate_all$per.s3 <- (substrate_all$s3.m2/substrate_all$total_habitat.m2)*100
substrate_all$per.s4 <- (substrate_all$s4.m2/substrate_all$total_habitat.m2)*100
substrate_all$total.rocks.per <- ((substrate_all$s4.m2 + substrate_all$s1.m2)/substrate_all$total_habitat.m2)*100

# calculate diversity, sum substrate 1 and 4 together because all rocks
library(vegan)
hab_types <- substrate_all[,c("s2.m2","s3.m2")]
hab_types$srock.m2 <- substrate_all$s1.m2+substrate_all$s4.m2
substrate_all$SW.div <- diversity(hab_types) 

write.csv(substrate_all, "/Users/efuller/1/CNH/processedData/spatial/ports/port_covariates/substrate_types/substrate_all_clean.csv")
