# finding centroid for species range based on trawl data. Average latitude and longitude of trawls for a species, multiplied by density at trawl so trawls that had large abundance have more pull on centroid

setwd("~/Documents/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Data/Malin_TrawlData/WC_Annual")

data <- read.csv('Merged_data.csv')

# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}
	
	spplist = aggregate(list(weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(weight=spplist$weight, pres = spplist$pres>0), by=list(spp=spplist$spp), FUN=sum) # pres col holds # years in which spp was present
	
	spplist = spplist[order(spplist$weight, decreasing=T),]
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 215 spp

# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=data$Trawl.Id), by=list(spp=data$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
# choose species that have most hauls, Microstomus pacificus (Pacific Dover Sole)
	
# Dover Sole
	Dover_Sole <- data[which(data$spp=="Microstomus pacificus"),]

	# plot hauls
	quartz(width=12, height=7)
	plot(data$lon,data$lat,pch=19,col="grey",cex=.5)
	points(Dover_Sole$lon, Dover_Sole$lat,pch=19,col="black",cex=.5)	# almost all hauls had this fish

	# calculate centroids
	require(cluster)
	Dover_2009 <- Dover_Sole[which(Dover_Sole$year == 9),]
	Dover_2010 <- Dover_Sole[which(Dover_Sole$year == 10),]
	
	# no data excluded
	nrow(Dover_2009) + nrow(Dover_2010) == nrow(Dover_Sole)

	medoids09 <- pam(Dover_2009[,9:10], 1)$medoids
	medoids10 <- pam(Dover_2010[,9:10], 1)$medoids

	points(medoids09[,2],medoids09[,1],pch=8,col="red",cex=2,lwd=2)
	points(medoids10[,2],medoids10[,1],pch=8,col="purple",cex=2,lwd=2)

# how do mean lat/lon compare?
	meanlon_09 <- mean(Dover_2009$lon)
	meanlat_09 <- mean(Dover_2009$lat)
	meanlon_10 <- mean(Dover_2010$lon)
	meanlat_10 <- mean(Dover_2010$lat)
	
	points(c(meanlon_09,meanlon_10),c(meanlat_09,meanlat_10),col=c("green","blue"),pch=4,lwd=2,cex=2)
	# hmm, doesn't look good.

# Shortspine thornyhead
 	Shortspine <- data[which(data$spp=="Sebastolobus altivelis"),]
 	
	quartz(width=12, height=7)
	plot(data$lon,data$lat,pch=19,col="grey",cex=.5)
	points(Shortspine$lon, Shortspine$lat,pch=19,col="black",cex=.5)	# almost all hauls had this fish
	
	Shortspine_2009 <- Shortspine[which(Shortspine$year == 9),]
	Shortspine_2010 <- Shortspine[which(Shortspine$year == 10),]
	
	# no data excluded
	nrow(Shortspine_2009) + nrow(Shortspine_2010) == nrow(Shortspine)

	medoids09 <- pam(Shortspine_2009[,9:10], 1)$medoids
	medoids10 <- pam(Shortspine_2010[,9:10], 1)$medoids

	points(medoids09[,2],medoids09[,1],pch=8,col="red",cex=2,lwd=2)
	points(medoids10[,2],medoids10[,1],pch=8,col="purple",cex=2,lwd=2)

# how do mean lat/lon compare?
	meanlon_09 <- mean(Shortspine_2009$lon)
	meanlat_09 <- mean(Shortspine_2009$lat)
	meanlon_10 <- mean(Shortspine_2010$lon)
	meanlat_10 <- mean(Shortspine_2010$lat)
	
	points(c(meanlon_09,meanlon_10),c(meanlat_09,meanlat_10),col=c("green","blue"),pch=4,lwd=2,cex=2)
	# hmm, doesn't look good.
