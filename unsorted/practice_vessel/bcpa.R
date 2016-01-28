# trying bcpa on vessel data

require(bcpa)
require(move)

one_boat <- read.table("/Users/efuller/Desktop/practice_vessel/wizard.txt",sep=",",header=F)
boat.dat <- one_boat[,c(3,4,5)]
names(boat.dat) <- c("Y","X","Time")
boat.dat$Time <- as.POSIXct(boat.dat$timestamp, "%Y-%m-%d %H:%M")

boat.VT <- GetVT(boat.dat)
hist(boat.VT$V, col = "grey",bor="darkgrey")
hist(boat.VT$Theta, col = "grey", bor="darkgrey")

boat.ws <- WindowSweep(boat.VT, "V*cos(Theta)", windowsize=100, K=2)
# doesn't work. 