# input: VMS.txt (csv of joined .log files)
# This is a script for loading the data into R and getting it into a proper format. What's left is to convert date-time into useful formats so they can be used to filter and plot things. 
rm(list = ls())

library(plyr); library(maptools); library(rgeos); library(rgdal)

###################################
# Reading and formatting Raw Data #
##################################
data <- read.csv("/Users/efuller/1/CNH/VMS_cleaning/data/VMS.txt",header=FALSE,strip.white=TRUE,stringsAsFactors=FALSE)

# since adding the file name to each row (for tracking purposes) There are more blank lines. To find these, search for column 3 NAs and remove
data <- data[-which(data[,3]==""),]

# convert latitude and longitude to decimal degrees. To do this I need to make a seperate column for seconds. 
lat_minutes <- as.numeric(data[,4]) # this generates some NAs in remnent header rows. remove them. 

# this generates NAs in a few more empty rows. These rows originally said something like 'xxxx rows selected." where xxx is a huge number. Now I can search for and remove any NAs in data$minutes
data <- data[-which(is.na(lat_minutes)),]

lat_minutes <- lat_minutes[-which(is.na(lat_minutes))]
lon_minutes <- as.numeric(as.character(data[,6]))

# Now I can make a column for seconds
lat_seconds <- lat_minutes - floor(lat_minutes)
lon_seconds <- lon_minutes - floor(lon_minutes)

# Now for decimal degrees. According to this website (http://andrew.hedges.name/experiments/convert_lat_long/) I can get decimal degrees by dividing the minutes by 60 and the seconds by 3600.
latitude <- as.numeric(as.character(data[,3])) + lat_minutes/60 + lat_seconds/3600
longitude <-as.numeric(as.character(data[,5])) - lon_minutes/60 - lon_seconds/3600
# because longitudes are all negative, we're dealing with the west coast

# clean up dataframe so only characters and numeric data
VMS <- data.frame(Vessel_Name = data[,1])
VMS$Ship_No <- as.character( data[,2])
VMS$Latitude <- latitude
VMS$Longitude <- longitude
VMS$Date_Time <- data[,7]
VMS$Avg_Speed <- as.numeric(as.character(data[,8]))
VMS$Avg_Direction <- as.numeric(as.character(data[,9]))
VMS$Declarations <- as.numeric(as.character(data[,10])) 
# this should generate some NAs because some declarations are NA

# next is to convert the dates and times into date and time objects, first need to split dates and times apart  Because they are currently in a format POSIX recognizes I can call POSIX directly
dt <- strptime(VMS$Date_Time, format='%Y-%m-%d %H:%M', tz = 'America/Los_Angeles' )

VMS$year = dt$year + 1900 # month, add 1 since POSIXlt starts at 0  
VMS$month = dt$mon+1 # month, add 1 since POSIXlt starts at 0	
VMS$day = dt$mday # day of the month
VMS$time = paste(formatC(dt$hour, width=2, flag=0), formatC(dt$min, width=2, flag=0), sep=':')

##################################################
# Merging Declarations and definitions with data #
##################################################

# next step is adding a lookup table to match Doc_number with ship_number. First need to import the lookup table
Vessel_Codes <- read.csv("/Users/efuller/1/CNH/Data/VMS/West_Coast_Vessels.csv",header=TRUE,stringsAsFactors=FALSE)

# changing the column name in my dataset to match lookup table
colnames(VMS)[2] = "Ship_Number"

VMS <- join(VMS, Vessel_Codes[,1:2], by = "Ship_Number")

saveRDS(VMS,"/Users/efuller/1/CNH/Analysis/VMS/writing/code/1_VMS_basic_clean.RDS" )
