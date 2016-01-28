#Various tools for a first exploratory analysis of the VMS data 



VMS <- read.csv('jittered_VMS.csv',sep=',') 
IDs <- length(unique(VMS[,2]))

#segment the data by vessel ID and -> get what is a season, essentially 
#for each i in IDs 
#season$i = data.frame([VMS[which(VMS[,2] == i)],])
################################################################3
# Legacy cleaning 
VMS_1 <- VMS[VMS$vessel_id==1,]
VMS_2 <- VMS[VMS$vessel_id==2,] 
VMS_3 <- VMS[VMS$vessel_id==3,]

#order by date, once segmented by vessel id
VMS_1 <- VMS_1[order(VMS_1$Date_Time),]
VMS_2 <- VMS_2[order(VMS_1$Date_Time),]
VMS_3 <- VMS_3[order(VMS_1$Date_Time),]

VMS_1_stats <- stats(VMS_1$Latitude,VMS_1$Longitude,VMS_1$Date_Time,VMS_1$onland,4)
VMS_2_stats <- stats(VMS_2$Latitude,VMS_2$Longitude,VMS_2$Date_Time,VMS_2$onland,4)
VMS_3_stats <- stats(VMS_3$Latitude,VMS_3$Longitude,VMS_3$Date_Time,VMS_3$onland,4)

#Isolate the indices of the times when the onland indicator is on
onland <- which(VMS_1$onland == 1) 
# Choose the indices where the fisher is not continuously on land
jumpsonland <- onland[which(diff(onland) != 1)]
lapply(jumpsonland,printDist) 



# Extract viable speeds 
# plot(VMS_1$Avg_Speed[(VMS_1$Avg_Speed > 0.4) & (VMS_1$onland == 0) & (VMS_1$Avg_Speed < 50)])
# plot(VMS_1$Avg_Speed[(VMS_1$Avg_Speed > 0.4) & (VMS_1$onland == 0) & (VMS_1$Avg_Speed < 50)])

#using the lubridate package to extract hour from POSIX
VMS_1_offland <- VMS_1[which(VMS_1$onland == 0),]
VMS_1_offland$hour <- hour(VMS_1_offland$Date_time) 
awake <- subset(VMS_1_offland, VMS_1_offland$hour > 5)


#more cleaning
awake <- awake[(!which(is.na(awake)),]
awake <- awake[(awake$Avg_Speed < 50),]
 

speeds <- as.numeric(VMS_1$Avg_Speed) 
plot(cumsum(as.numeric(speeds[1:2800])),xlab="Measurement #",ylab="Cumulative Speed from VMS data")
plot(cumsum(VMS_1$Avg_Speed[2000:7500]),xlab="index",ylab="Cumulative Sum of speed",main="Vessel 3, overview of cumulative speed", col=as.factor(VMS_1$onland))

##################################
# SPLITTING THE DATA FRAME INTO SEPARATE TRIPS 
# Return the indices of VMS speeds not consecutively the same for 5 times
# This line will aggregate all the nonconsecutive cum speed together: 
# notconsec_VMS1 <- VMS_1[!notconsec_VMS1_Ind$Ind,]

# Use this on cumulative sum of speed
# FIRST: clean the NA values 
VMS_1$Avg_Speed[which(is.na(VMS_1$Avg_Speed))] <- 0 
#
VMS_1$Cum_Speed <- cumsum(VMS_1$Avg_Speed) 
VMS_1$trips <- 0 
notconsec_VMS1_Ind <- not_consec(VMS_1$Cum_Speed, 20)
# find breakpoints
notconsec_breaks <- notconsec_VMS1_Ind$Ind[which(diff(notconsec_VMS1_Ind$Ind) != 1)]

# need to split on these repeats  
# doing it the brute force way 

# if difference between diffed vector is > 1 
break_ind <- diff(notconsec_VMS1_Ind$Ind) 
for (i in 1:(length(notconsec_VMS1_Ind$Ind)-1)) {
# write i to the interval vect$ind[breaks[i]:breaks[i+1]] 
# else write 0 
	if (break_ind[i] != 1) {
		VMS_1$trips[notconsec_VMS1_Ind$Ind[i]:notconsec_VMS1_Ind$Ind[i+1]] <- i 
	}
	else {
		VMS_1$trips[notconsec_VMS1_Ind$Ind[i]] <- 0 
	}
}

# split on this matrix
VMS_1_split <- split(VMS_1, VMS_1$trips) 




### check for missing values
####################
####### NOTE: should properly be wrapped in a function with rm or not as a flag 
for (i in 2:length(VMS_1_split)) {
	df <- VMS_1_split[[i]]
	# check by column 
	for (j in 1:dim(df)[2]) {
		derp <- which(is.na(df[,j])) # check each column for missing values 
		if (length(derp) > 0) {
		cat("Missing at iteration", i, ", column: ", j, ": ", derp,"\n")  # print for debugging purposes 
		VMS_1_split[[i]] <- df[-derp,] # Remove the offending row 
		#################################
		# NOTE: It looks like the missing values are mostly headings, which could potentially be calculated from position data
		# TODO - more proper missing data 
		}
	}
}

# iterate through again and check that everything was removed 
for (i in 2:length(VMS_1_split)) {
	df <- VMS_1_split[[i]]
	# check by column 
	for (j in 1:dim(df)[2]) {
		derp <- which(is.na(df[,j])) # check each column for missing values 
		if (length(derp) > 0) {
		cat("Missing at iteration", i, ", column: ", j, ": ", derp,"\n")  # print for debugging purposes 
		}
	}
}

########################
# Compute the cubic hermite spline interpolation for each trip and write to a list of 
# such data frames 
# CRmInterpolate is modified catmull-rom from Russo, Parisi, Cataudella, 2011
# 
# resolution: # minutes between observations
resolution <- 10
interpolated_list <- list() 
for (i in 2:length(VMS_1_split)) {
	df <- VMS_1_split[[i]]
	interpolated_list[[i]] <- CRmInterpolate(df$Latitude,df$Longitude,df$Avg_Direction,df$Avg_Speed,10,df$Date_Time)
	#print(i) #print for diagnostic purposes
}
# first element is data frame composed of the consecutively 0 speed states 
# need to index with double square brackets 
stats_list <- lapply(interpolated_list, plystats) 

test <- CRmInterpolate(VMS_1_split[[3]]$Latitude,VMS_1_split[[3]]$Longitude,VMS_1_split[[3]]$Avg_Direction,VMS_1_split[[3]]$Avg_Speed,10,VMS_1_split[[3]]$Date_Time)


#################
#################
################# MISCELLANEOUS OLD TESTING CODE
output <- clustCombi(awake$Avg_Speed) 


night <- (notconsec_VMS1$hour > 21 | notconsec_VMS1$hour < 5)
notconsec_day_VMS1 <- subset(notconsec_VMS1,notconsec_VMS1$night == FALSE)

plot(cumsum(notconsec_day_VMS1$Avg_Speed[1:500]), col=as.factor(notconsec_day_VMS1$night)) 