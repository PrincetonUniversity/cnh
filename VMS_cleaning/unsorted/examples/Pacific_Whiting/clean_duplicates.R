# This script pulls out Pacific whiting fisheries and looks for duplicates
## 221: Limited entry midwater trawl gear, Pacific whiting shorebased IFQ
## 222: Limited entry midwater trawl gear, Pacific whiting catcher/processor sector
## 223: Limited entry midwater trawl gear, Pacific whiting mothership sector (catcher vessel or mothership)

# read in incomplete data
VMS <- read.csv("/home/efuller/NOAA/Data_Analysis/Code/processed_data/VMS_wDups.csv")
	
	# read in substitute data - incomplete VMS data, only for substitute use of local machine
	VMS <- read.csv("/Users/efuller/Documents/Projects/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Code/processed_data/sorted.Rda")
	
VMS$Date_Time <- as.POSIXct(strptime(VMS$Date_Time, format='%Y-%m-%d %H:%M', tz = 'America/Los_Angeles' ))

shorebased <- VMS[which(VMS$Declaration==221),]
catcherprocessor <- VMS[which(VMS$Declaration==222),]
mothership <- VMS[which(VMS$Declaration==223),]


# mapping
require(maps)
require(mapdata)

bestcoast <- map_data("usa")

# trajectory analysis
require(adehabitatLT)

shorebased_trj <- as.ltraj(xy = shorebased[,c("Longitude","Latitude")], date = shorebased$Date_Time,  id = as.character(shorebased$Vessel_Name),typeII=FALSE)

# first step is to go one-by-one for each vessel and look at which points are dups and which are not (should just append dist, R2n, abs.angle and rel.angle to dataframe for each vessel trajectory)
ARCTICFURY <- shorebased[which(shorebased$Vessel_Name=="ARCTIC FURY"),]
ARCTICFURY <- ARCTICFURY[order(ARCTICFURY$Date_Time),]

ARCTIC_trj <- as.ltraj(xy = ARCTICFURY[,c("Longitude","Latitude")], date=ARCTICFURY$Date_Time, id=as.character(ARCTICFURY$Vessel_Name))

# look to see time between relocations
png(file = "../Code/inst/examples/Pacific_Whiting/Plots/ArcticFury_full.png")
plotltr(ARCTIC_trj, "dt/3600/24")
dev.off()

# can see big breaks. Should break trajectories if break between relocations is more than 2 hours. First define a function that returns TRUE when the time lag between two successive relocations is greater than 2 hours

lag <- function(dt) {
    return(dt > (3600*2))
}

# use cutltraj to cut burst locations with a value of dt such that lag(dt) is true, into several bursts for which no value of dt fullfills this criterio
ARCTIC_trj2 <- cutltraj(ARCTIC_trj, "lag(dt)", nextr = TRUE)

png(file = "home/efuller/NOAA/Data_Analysis/Code/inst/examples/Pacific_Whiting/Plots/ArcticFury_split.png")

plotltr(ARCTIC_trj2[[1]], "dt/3600/24")
dev.off()

