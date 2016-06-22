# exploration of revenue - are there clusters of people that make about the same amount? clear breakpoints?

ftl <- read.csv(
  "/Users/efuller/1/CNH/Data/Catch/FTL_2009-2013_w-o-c_samhouri.csv", 
  stringsAsFactors = F)

library(dplyr)
# remove bad vessels
ftl = ftl[-grep("[***]", ftl$veid),]
ftl <- ftl[-which(ftl$veid=="0"),]
ftl <- ftl[-which(ftl$veid=="UNKNOWN"),]

# calculate annual revenue
vessels <- group_by(ftl, veid, year)
yearly_revenue <- summarise(vessels, revenue = sum(landed_wt * ppp))

# find average revenue
boats <- group_by(yearly_revenue, veid)
average_revenue <- summarise(boats, average_rev = mean(revenue), sd_rev = sd(revenue))

# remove vessels with only one datapoitn
average_revenue <- average_revenue[-which(is.na(average_revenue$sd_rev)),]

# kmeans clustering 
library(cluster)



cl <- list()
sil <- vector()
for(i in 2:10){
  cl[[i]] <- pam(average_revenue$average_rev, k = i)
  sil[i] <- cl[[i]]$silinfo$avg.width
}

cl <- kmeans(average_revenue$average_rev, 5, nstart = 25)
plot(average_revenue$average_rev, col=cl$cluster,pch=3, cex=.75)
points(cl$centers, col=1:5, pch=8, cex=2)
