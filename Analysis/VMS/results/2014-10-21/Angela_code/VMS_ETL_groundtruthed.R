# Load dependencies
library(mclust)
library(class)
library(lubridate) 
library(bcpa)
library(strptime)

library(mhsmm)

## Helper functions applied with lapply

#re-order data entries by datetime
order_by_date <- function(df) {
	df <- df[order(df$Date_Time),]

}
# 
# extract offland points
which_offland <- function(df) {
df <- df[which(df$onland == 0),] 
}

############################################
# data-cleaning and throat clearing 
#
GT_VMS <- read.csv("anonymized_groundtruthed.csv")
# Parse the date-time data into POSIXct
GT_VMS$Date_Time <- as.POSIXct(GT_VMS$Date_Time, format="%Y-%m-%d %H:%M:%S", tz="US/Pacific")
GT_VMS_vessels <- split(GT_VMS, GT_VMS$vessel_id) 
GT_VMS_vessels <- lapply(GT_VMS_vessels,order_by_date) 
# keep the offland points 
GT_VMS_vessels_offland <- lapply(GT_VMS_vessels, which_offland)
GT_VMS_vessels_observed <- lapply( GT_VMS_vessels, function(df) { df <- df[which(df$observed == 1),]} )


# just renaming the cleaned set 
GT_VMS <- GT_VMS_vessels_offland

# split the set into observed segments 
GT_VMS_obs <- list(length(GT_VMS))
GT_VMS_obs_total <- lapply(GT_VMS, observed_trips, n=3)

## GT_VMS_obs_total is a list of (vessels) list of data frames (observer trips), which is confusing 
# Discard the first frame in each list in GT_VMS_obs_total, since that's where the non-observed data is 
GT_VMS_obs <- lapply(GT_VMS_obs_total, function(list) list <- list[-1])
# HACK/TO-DO: drop trips of less than 4 observations 

for (i in 1:length(GT_VMS_obs)) {
		for ( j in 1:length(GT_VMS_obs[[i]])) {
			if (length(GT_VMS_obs[[i]][[j]]$trips) < 4) 
				GT_VMS_obs[[i]][[j]] <- NULL
}
}
##########################################################################



##########################################################################
# Applying the spatial statistics and analysis 

# Interpolate each trip: CRm, modified Catmull-Rom (modified cubic Hermite spline) 
# as found in VMS_interpolate.R 
### This lapply should work, but it's not, so i'll just use a for loop 
# GT_VMS_interp <- lapply( GT_VMS_obs, interpolate_list(x), resolution = 10 )

# check for missing values 
lapply(GT_VMS_obs,which_isna_iterate)

# Handling NA then is decided manually 
# I found I was getting NA values in columns 3-6, which are Date_Time, Avg_Speed, Avg_Direction, status; 
# chose to remove observations corresponding to Date_Time, Avg_Speed, Avg_Direction

# Run the interpretation on all frames 

GT_VMS_interp <- list() 
for (i in 1:length(GT_VMS_obs)) {
	try ( GT_VMS_interp[[i]] <- interpolate_list(GT_VMS_obs[[i]],10) )
	GT_VMS_interp[[i]]$tripID <- GT_VMS_obs[[i]]$trips[1] 
}

GT_VMS_stats <- lapply(GT_VMS_interp, stats_list_VMS)



###########################################################################
###########################################################################
# Runnign the exploratory algorithms
###########################################################################

# #write a list of KNN objects from stats - VC, VS, Theta, S 
# KNN_caller <- function(df) {
# 	stats <- stats_BCPA_regular_notrain(complex(re = df[,1], im = df[,2]),df[,3])
# 	# truncate because we're missing a data point on 
# 	stats <- data.frame(stats[[2]], stats[[3]][-1],stats[[5]],stats[[6]])
# 	#statsEM <- Mclust(stats,2:2,c("VVV"))
# 	knnlapply <- (kmeans(stats, 2))
# 	return(knnlapply)
# }
# list_KNN_caller <- function(list) { 
# 	results <- lapply(list, KNN_caller) 
# 	return(results)
# }

# BCPA_caller <- function(df) {
# 	bcpa_list <- stats_BCPA_VMS(complex(re=df[,1],im=df[,2]), df[,3],3)
# 	return(bcpa_list)
# }
# list_BCPA_caller <- function(list) {
# 	BCPA_list <- lapply(list, BCPA_caller)
# 	return(BCPA_list) 
# }


EM_results <- list()
for ( i in 1:length(GT_VMS_interp)) {
	for ( j in 1:length(GT_VMS_interp[[i]])) {
		df <- GT_VMS_interp[[i]][[j]]
		stats <- stats_BCPA_regular_notrain(complex(re = df[,1], im = df[,2]),df[,3])
		# truncate because we're missing a data point on 
	#	stats <- data.frame(stats[[2]], stats[[3]][-1],stats[[5]],stats[[6]])
		EM_results[[i]][[j]] <- Mclust(stats,2:2,c("VVV"))
	}
}
EM_results_obs <- list()
for ( i in 1:length(GT_VMS_obs)) {
	EM_results_obs[[i]] <- list()
 	for ( j in 1:length(GT_VMS_obs[[i]])) {
		df <- GT_VMS_obs[[i]][[j]]
		stats <- stats_BCPA_VMS_notrain(complex(re = df$Longitude, im = df$Latitude),df$Date_Time)
		# truncate because we're missing a data point on 
		stats <- data.frame(stats[[2]], stats$S[-1],stats[[5]],stats[[6]])
		try( results <- Mclust(stats,G = 2:2) )
		EM_results_obs[[i]][j] <- results
	}
}

KNN_results <- list()
for ( i in 1:length(GT_VMS_interp)) {
	for ( j in 1:length(GT_VMS_interp[[i]])) {
		df <- GT_VMS_interp[[i]][[j]]
		stats <- stats_BCPA_regular_notrain(complex(re = df[,1], im = df[,2]),df[,3])		
		# truncate because we have an extra data point on 3
		stats <- data.frame(stats[[2]], stats[[3]][-1],stats[[5]],stats[[6]])

		KNN_results[i][j] <- kmeans(stats, centers = 2)
	}
}

KNN_obs_results <- list()
for ( i in 1:length(GT_VMS_obs)) {
	for ( j in 1:length(GT_VMS_obs[[i]])) {
		df <- GT_VMS_obs[[i]][[j]]
		stats <- stats_BCPA_VMS_notrain(complex(re = df$Longitude, im = df$Latitude),df$Date_Time)		
		# truncate because we're missing a data point on 
		# here we're using turning angle, step length, VC, VS
		stats <- data.frame(stats$Theta, stats$S[-1],stats$VC,stats$VS)

		KNN_results[i][j] <- list(kmeans(stats, centers = 2), df$trips[1])
	}
}

# write a list of BCPA classifications
BCPA_results_update <- list()
for ( i in 1:length(GT_VMS_interp)) {
	for ( j in 1:length(GT_VMS_interp[[i]])) {
		print(i*length(GT_VMS_interp) + j)
		df <- GT_VMS_interp[[i]][[j]]
		BCPA_results[i*length(GT_VMS_interp)+j] <- list(stats_BCPA_regular(complex(re=df[,1],im=df[,2]), df[,3],3), df$tripID)
	}
}



# HSMM 
# seed the initial transition matrix 
# For single-variable observations
# tpmpar <- matrix(c(0,0.8,0.8,0), 2, byrow = TRUE)
# odpar <- list("mean" = c(0,0.1), "var" = c(0.1,0.1))
# pi <- c(0.5,0,5) #Seed the initial probabilities arbitrarily 
# HSMM <- hsmm.viterbi( c(HSMM_interp_stats$VC,HSMM_interp_stats$VS) , od = "norm", rd = "pois", pi.par = pi, rd.par = list("lambda" = c(24,12)), od.par = odpar, tpm.par = tpmpar)
# # EM 

#Training HSMM on multivariate data 
init <- c(.5,.5) #initialize with not fishing
J <- 2
P <- matrix ( c(0,1,1,0), nrow = J)
B <- list(mu = c(0,0.1), sigma=c(0.1,0.1) )
d <- list("shift" = c(10,10), "lambda" = c(36,12),type="poisson" )
model <- hsmmspec(init, transition = P, parms.emission = B, sojourn = d, dens.emission=dnorm.hsmm )
M <- 300
hsmm <- hsmmfit( stats[1], model, mstep = mstep.norm, lock.transition = TRUE, M = 300)
predicted <- predict(hsmm, stats) 
table(stats$s, predicted$s) ##classification matrix
mean(predicted$s != train$s) ## misclassification rate


