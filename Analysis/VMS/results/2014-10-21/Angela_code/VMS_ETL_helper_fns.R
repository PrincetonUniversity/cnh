# File of private helper functions used in various other calculations for
# cleaning VMS data 

# gcd.hf: great-circle distance calculation
# printDist: diagnostic method for checking distances between VMS data points

###########################################################
# Various useful one-liners

heading2unitvec <- function(x) cbind(cos(deg2rad(x)),sin(deg2rad(x)))
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
deg2rad <- function(deg) return(deg*pi/180)
norm_vec <- function(x) sqrt(sum(x^2)) 

# Great Circle Distance Calc in R, using Haversine for small-scale computational stability. 
# http://www.r-bloggers.com/great-circle-distance-calculations-in-r/
# Calculates the geodesic distance between two points specified by #DEGREE# latitude/longitude using the
# Haversine formula (hf)
# Accepts measurements in degrees, converts them to radians
gcd.hf <- function(long1, lat1, long2, lat2) {
  deg2rad <- function(deg) return(deg*pi/180)
  long1 <- deg2rad(long1)
  lat1 <- deg2rad(lat1) 
  long2 <- deg2rad(long2) 
  lat2 <- deg2rad(lat2) 

  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

# Helper function to remove values repeated consecutively N times. 
# Returns a vector of the indices to keep 
not_consec <- function(X, N)
{
 .rle <- rle(sort(X))
 res <- .rle$values[.rle$lengths >= N]
 res <- res[res > 0]
 inds <- X %in% res
 X[inds] <- NA 
 list(X = X, Ind = which(inds)) 
}



#Check the distances jumped by each 
#Helper function that doesn't work for bounday cases (first and last element) 
printDist <- function(ind) {
	dist <- gcd.hf(VMS_1$Longitude[ind-1],VMS_1$Latitude[ind-1],VMS_1$Longitude[ind],VMS_1$Latitude[ind])
	print(dist)
	diff <- VMS_1$Date_Time[ind] - VMS_1$Date_Time[ind-1]
	if (diff < 1) print(diff)
}

