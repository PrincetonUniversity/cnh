# using one vessel from VMS. This is the boat that had the most trips in the "shrimp" catch profile. This data was grep-ed from VMS_woDups.csv using the veid: grep 522171 /Volumes/NOAA_Data/CNH/VMS_cleaning/results/2014-03-02/VMS_woDups.csv

require(scales)
require(mapdata)
require(maps)

## Load Data

  one_boat <- read.table("/Users/efuller/Desktop/practice_vessel/wizard.txt",sep=",",header=F)
  boat.dat <- one_boat[,c(3,4,5)]
  names(boat.dat) <- c("y","x","timestamp")

  # NOTE
  # may need to reproject my data. It's originally in lat/lon minutes/seconds.

## Convert to complex variable

  Z = boat.dat$x+1i*boat.dat$y

## Obtain a vector of time stamps T, draw a histogram of the time intervals, then ignore these differences
  
  # make time formats work
    boat.dat$timestamp <- as.POSIXct(boat.dat$timestamp, "%Y-%m-%d %H:%M")
  # make time intervals
    dt <- difftime(boat.dat$timestamp[1:(nrow(boat.dat)-1)], boat.dat$timestamp[2:nrow(boat.dat)])
  # make a histogram, see a summary
    hist(as.numeric(dt), col="grey",bor="darkgrey",freq=FALSE)
    summary(as.numeric(dt))

## Obtain, summarize and illustrate the step lengths, absolute orientations, and the turning angles
  
  dZ <- diff(Z)
  
  # steplength (doesn't mean much if the interval of time is not regular)
    S <- Mod(dZ)
    hist(S, col="grey", bor="darkgrey",freq=FALSE,breaks=30)
    lines(density(S),col="red",lwd=2)
    summary(S)

  # to get speed as estimated by displacement over time
    #Speed <- S/a

  # absolute orientations
    Phi <- Arg(dZ)
    hist(Phi, col="grey",bor="darkgrey",freq=FALSE)
    lines(density(Phi),col="red",lwd=2)

  # turning angles
    Theta <- diff(Phi)
    hist(Theta, col="grey",bor="darkgrey",freq=FALSE)
    lines(density(Theta), col="red", lwd=2)


# try estimating parameters for weibull parameters with MLE

# likelihood function
Weibull.Like <- function(p, Z){
  S = Mod(diff(Z))
  -sum(dweibull(S, p[1], p[2], log = TRUE))
}

(Weibull.fit <- optim(c(1,1), Weibull.Like, Z=Z2))

hist(Mod(diff(Z2)), freq=FALSE, col="grey", breaks=10,bor="darkgrey")
curve(dweibull(x, Weibull.fit$par[1], Weibull.fit$par[2]), add=TRUE, col=2, lwd=2)

# try building a null set

  subZ <- Z[1400:3900]
  n <- length(subZ)
  S <- Mod(diff(subZ))
  Phi <- Arg(diff(subZ))
  Theta <- diff(Phi)
  RelSteps <- complex(mod=S[-1],arg=Theta)

  # calculate null set
    Z0 <- subZ[-((n-1):n)]
    Z1 <- subZ[-c(1,n)]
    Rotate <- complex(mod =1, arg=Arg(Z1-Z0))
    Z.null <- matrix(0,ncol=n-2,nrow=n-2)
    for(i in 1:length(Z1)){
      Z.null[i,] <- Z1[i] + sample(max(length(RelSteps),30)) * Rotate[i]
    }
  
  # plot
    plot(subZ, type="o", pch=19, asp=1)
    for(i in 1:nrow(Z.null)){
      segments(rep(Re(Z1[i]), n-2), rep(Im(Z1[i]), n-2), Re(Z.null[i,]), Im(Z.null[i,]))
    }



