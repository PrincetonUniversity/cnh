# K-means clustering to determine differences between traveling, fishing and in port. Using declarations as proxy for each fishery sector. 

# using this website as a guide: http://www.r-statistics.com/2013/08/k-means-clustering-from-r-in-action/

library(stats)
library(sp)
library(maps)
library(mapdata)
library(scales)

# if ursus
setwd("/home/efuller/NOAA/Data_Analysis/Code/processed_data")
if(!exists("VMSdata")) {VMSdata <- readRDS("processedVMS.RDS")}


# just on-water, non port data
VMSdata <- subset(VMSdata, onland==0 & is.na(status))


# function to plot the sum of squares as number of clusters increases. Best number of clusters is the biggest drop in sum of squares
wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")}

# will use k-means clustering on individual fishermen to determine difference between in port, steaming and fishing. 

# vector of ship docs
Ships <- unique(VMSdata$Ship_Number)
Names <- as.character(unique(VMSdata$Vessel_Name))

# take a single fishermen out to try
Ruby <- subset(VMSdata, Ship_Number==Ships[1])
Ruby_Dist <- as.matrix(cbind(Ruby$Longitude,Ruby$Latitude))

# make column of change in distance between two points and difference in heading
Kmeans <- data.frame(Distance = spDistsN1(Ruby_Dist,Ruby_Dist[1,]), Diff_heading = c(NA,diff(Ruby$Avg_Direction)),Longitude = Ruby$Longitude, Latitude=Ruby$Latitude)

## problem: there are missing values for change in heading. Clustering analysis has no good way to deal with this. Either requires we take mean and input that (but that assumes that these imputed values are as reliable as actual recorded ones) or we remove those values.

## here I remove them

Kmeans <- Kmeans[-which(is.na(Kmeans$Diff_heading)),]

cl <- kmeans(Kmeans[,1:2],3)    # 3 clusters

par(mfrow=c(1,2))
plot(Kmeans[,1:2],col=cl$cluster)
points(cl$centers,col=1:4,cex=5,pch=8,lwd=2)

map("worldHires","USA", xlim=range(Ruby$Longitude)+c(-.15,.15), ylim=range(Ruby$Latitude)+c(-.15,.15),fill=TRUE,col="grey",border=FALSE)
points(Kmeans$Longitude, Kmeans$Latitude, col=cl$cluster)
lines(Kmeans$Longitude, Kmeans$Latitude, col=alpha("grey",0.5))

# this makes it look like that if we took the absolute difference of heading,. I don't think it matters which way they're turning, just how much they're turning. 

Kmeans_abs <- Kmeans
Kmeans_abs$Diff_heading <- abs(Kmeans_abs$Diff_heading)
wssplot(Kmeans_abs)
cl_1 <- kmeans(Kmeans_abs[,1:2],2)
plot(Kmeans_abs[,1:2],col=cl_1$cluster)
points(cl_1$centers,col=1:2,cex=5,pch=8,lwd=2)
map("worldHires","USA", xlim=range(Ruby$Longitude)+c(-.15,.15), ylim=range(Ruby$Latitude)+c(-.15,.15),fill=TRUE,col="grey",border=FALSE)
points(Kmeans_abs$Longitude, Kmeans_abs$Latitude, col=cl_1$cluster)
lines(Kmeans_abs$Longitude, Kmeans_abs$Latitude, col=alpha("grey",0.5))

# try just lat and lon (maybe all port ones will be clustered)

cl_2 <- kmeans(Kmeans[,3:4],3)

# check plot
map("worldHires","USA", xlim=range(Ruby$Longitude)+c(-.15,.15), ylim=range(Ruby$Latitude)+c(-.15,.15),fill=TRUE,col="grey",border=FALSE)
points(Kmeans$Longitude, Kmeans$Latitude, col=cl_2$cluster)
lines(Kmeans$Longitude, Kmeans$Latitude, col=alpha("grey",0.5))

wssplot(Kmeans[,3:4])    # this looks really good for 2-4 clusters (u-bend in graph)

# what about using the entire dataset (but absolute change in heading)
wssplot(Kmeans_abs)	# so trying with 2 clusters
cl_3 <- kmeans(Kmeans,2)
map("worldHires","USA", xlim=range(Ruby$Longitude)+c(-.15,.15), ylim=range(Ruby$Latitude)+c(-.15,.15),fill=TRUE,col="grey",border=FALSE)
points(Kmeans$Longitude, Kmeans$Latitude, col=cl_3$cluster)
lines(Kmeans$Longitude, Kmeans$Latitude, col=alpha("grey",0.5))

# still doesn't look good. Try a second boat

########################
## Ship #2 in dataset ##
########################

Victory <- subset(VMSdata, Ship_Number==Ships[2])

map("worldHires","USA", xlim=c(-130,-118), ylim=c(40,47),fill=TRUE,col="grey",border=FALSE)
lines(Victory$Longitude, Victory$Latitude, col=alpha("blue",0.5),type="o",pch=19)

# this guy just heads out to sea. Try the next boat

########################
## Ship #3 in dataset ##
########################

Lawrence <- subset(VMSdata, Ship_Number==Ships[3])

map("worldHires","USA", xlim=c(-125,-121.5), ylim=c(37,42),fill=TRUE,col="grey",border=FALSE)
lines(Lawrence$Longitude, Lawrence$Latitude, col=alpha("blue",0.5),type="o",pch=19)

Lawrence_Dist <- as.matrix(cbind(Lawrence$Longitude,Lawrence$Latitude))


Kmeans <- data.frame(Distance = spDistsN1(Lawrence_Dist,Lawrence_Dist[1,]), Diff_heading = c(NA,diff(Lawrence$Avg_Direction)),Longitude = Lawrence$Longitude, Latitude=Lawrence$Latitude)

Kmeans <- Kmeans[-which(is.na(Kmeans$Diff_heading)),]


wssplot(Kmeans[,1:2])   # looks like 4, but let's try 3

cl_4 <- kmeans(Kmeans[,1:2],3)
map("worldHires","USA", xlim=c(-125,-121.5), ylim=c(37,42),fill=TRUE,col="grey",border=FALSE)
lines(Kmeans$Longitude,Kmeans$Latitude,pch=19,type="o",col=alpha(cl_4$cluster,0.5))

# again not sure about this. 

########################
## Ship #4 in dataset ##
########################

Genieve_M <- subset(sorted, Ship_Number==Ships[4])
map("worldHires","USA", xlim=c(-125,-117), ylim=c(37,42),fill=TRUE,col="grey",border=FALSE)
lines(Genieve_M$Longitude, Genieve_M$Latitude, col=alpha("blue",0.5),type="o",pch=19)

# another long term exemption.

########################
## Ship #5 in dataset ##
########################
Nikki_J <- subset(sorted, Ship_Number==Ships[5])
map("worldHires","USA", xlim=range(Nikki_J$Longitude)+c(-.5,.5), ylim=range(Nikki_J$Latitude)+c(-.5,.5),fill=TRUE,col="grey",border=FALSE)
lines(Nikki_J$Longitude, Nikki_J$Latitude, col=alpha("blue",0.5),type="o",pch=19)

########################
## Ship #6 in dataset ##
########################
Millennium <- subset(sorted, Ship_Number==Ships[6])
map("worldHires","USA", xlim=range(Millennium$Longitude)+c(-5,5), ylim=range(Millennium$Latitude)+c(-5,5),fill=TRUE,col="grey",border=FALSE)
lines(Millennium$Longitude, Millennium$Latitude, col=alpha("blue",0.5),type="o",pch=19)

########################
## Ship #7 in dataset ##
########################
Sunset_Charge <- subset(sorted, Ship_Number==Ships[7])
map("worldHires","USA", xlim=range(Sunset_Charge$Longitude)+c(-1,1), ylim=range(Sunset_Charge$Latitude)+c(-1,1),fill=TRUE,col="grey",border=FALSE)
lines(Sunset_Charge$Longitude, Sunset_Charge$Latitude, col=alpha("blue",0.5),type="o",pch=19)

# seems like these "long-term departure exemptions are sure doing a lot of fishing. 

########################
## Ship #8 in dataset ##
########################

Fish_on <- subset(sorted,Ship_Number==Ships[8])
map("worldHires","USA", xlim=range(Fish_on$Longitude)+c(-.1,.1), ylim=range(Fish_on$Latitude)+c(-.1,.1),fill=TRUE,col="grey",border=FALSE)
lines(Fish_on$Longitude, Fish_on$Latitude, col=alpha("blue",0.5),type="o",pch=19)
