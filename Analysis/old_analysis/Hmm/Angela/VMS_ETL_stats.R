# Dependencies: VMS_ETL_helper_fns.R
#helper wrapper for stats for use in apply/sapply/lapply 

plystats <- function(df) {
	win <- 3
	stats(df$Latitude,df$Longitude,df$Date_Time,df$onland,win)
}

stats <- function(x,y,tm,onland,win) {
	n = length(x)
	# Can be rewritten/compiled in C later for speed purposes.. 
	# iterate over number of rows in dataset
	speeds <- rep(0,n); 
	angles <- rep(0,n); 
	dists <- rep(0,n); 
	
	#for sinuosity
	slidingC <- rep(0,n); p <- rep(0,n); b <- rep(0,n); 
	lagDist <- rep(0,n); 
	sumDist <- 0; 

	vect1 <- rbind(0,0)
	for (i in (win+1):(n-1)) {
		if (onland[i] == 0) {
		vect2 <- c(x[i+1] - x[i], y[i+1] - y[i])
		dists[i] <- gcd.hf(x[i+1],y[i+1],x[i],y[i])
		#Assume time is given as POSIXct
		timediff <- difftime(tm[i],tm[i-1],units="hours")
		speeds[i] <- dists[i] / as.numeric(timediff)
		

		angles[i] <- atan(vect1[2]/vect2[1]) - atan(vect1[2] / vect1[1] )	
		if (sqrt(sum(vect2^2)) == 0) angles[i] = 0 

		slidingC[i] <- mean(cos(angles[(i-win):i]))
		p[i] <- mean(dists[(i-win):i]) 
		b[i] <- sd(dists[(i-win):i ])

		expDist <- dist(rbind(c(x[i],y[i]),c(x[i-win],y[i-win])))
		lagDist[i] <- expDist / sum(dists[(i-win):i])
		if (expDist == 0) lagDist[i] = 0

		vect1 <- vect2

		}
	}

	sinuosity <- rep(0,n)
	for (i in (win+1):n) {
		if (onland[i] == 0) {
		ratio <- (1 + slidingC[i]) / (1 - slidingC[i])
		sinuosity[i] = 2 / sqrt(p[i] * ratio + b[i]*b[i])
		}
	}
	#retList <- list("speeds" = speeds, "angles" = angles, "sinuosity" = sinuosity, "lagDist" = lagDist, "meanCos" = slidingC)
	retList <- data.frame("speeds" = speeds, "angles" = angles, "sinuosity" = sinuosity, "lagDist" = lagDist, "meanCos" = slidingC)

	return(retList)
}