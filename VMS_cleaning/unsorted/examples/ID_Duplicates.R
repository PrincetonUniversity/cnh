# was trying to calculating turning angles in radians for all trajectories, but couldn't load data properly into adehabitat package. Looks like there are duplicate data records. 
library(adehabitatLT)
library(sp)
etwd("~/NOAA/Data_Analysis/Code/processed_data")

if(!exists("VMSdata")){VMSdata <- readRDS("processedVMS.RDS")}

foo <- VMSdata[100000:200000,]

# there appears to be duplicates in my dataset
dupes <- foo$Date_Time[duplicated(foo$Date_Time)]

# wrong, somehow the dates and times aren't being recorded properly and different days are being assumed as the same ones. 

foo.time <- foo
foo.time$Date_Time <- as.numeric(foo.time$Date_Time)

traj_foo <- as.ltraj(xy=c(foo.time$Longitude, foo.time$Latitude), date=as.POSIXct(foo.time$Date_Time), id = as.character(foo.time$Vessel_Name))


# some problem with the dates and times. somehow not picking up different days, or years. Unclear, can't figure out bug. Trying just one vessel

NikkJ <- VMSdata[which(VMSdata$Vessel_Name=="Nikki J"),]

dups <- which(duplicated(NikkJ$Date_Time))

# still a problem

length(NikkJ$Date_Time) == length(unique(NikkJ$Date_Time))

which(NikkJ$Date_Time==dups[1])

unique_NikkJ <- NikkJ[!duplicated(NikkJ),]

length(unique_NikkJ$Date_Time) == length(unique(NikkJ$Date_Time))

# does look like duplicates
NikkJ[which(NikkJ$Date_Time==dups[5]),1:5]

# go check original data file and figure out if there are duplicates there

data <- read.csv("~/NOAA/Data_Analysis/Data/VMS.txt",header=FALSE,strip.white=TRUE,stringsAsFactors=FALSE)

# since adding the file name to each row (for tracking purposes) There are more blank lines. To find these, search for column 3 NAs and remove
data <- data[-which(data[,3]==""),]
# convert latitude and longitude to decimal degrees. To do this I need to make a seperate column for seconds..

lat_minutes <- as.numeric(data[,4])    # this generates some NAs in remnent header rows. remove them..

# this generates NAs in a few more empty rows. These rows originally said something like 'xxxx rows selected." where xxx is a huge number. Now I can search for and remove any NAs in data$minutes

data <- data[-which(is.na(lat_minutes)),]

lat_minutes <- lat_minutes[-which(is.na(lat_minutes))]
lon_minutes <- as.numeric(as.character(data[,6]))

# Now I can make a column for seconds

lat_seconds <- lat_minutes - floor(lat_minutes)
lon_seconds <- lon_minutes - floor(lon_minutes)

# Now for decimal degrees. According to this website (http://andrew.hedges.name/experiments/convert_lat_long/) I can get decimal degrees by dividing the minutes by 60 and the seconds by 3600.

latitude <- as.numeric(as.character(data[,3])) + lat_minutes/60 + lat_seconds/3600
longitude <-as.numeric(as.character(data[,5])) - lon_minutes/60 - lon_seconds/3600  # because longitudes are all negative, we're dealing with the west coast

# clean up dataframe so only characters and numeric data
VMS <- data.frame(Vessel_Name = data[,1])
VMS$Ship_No <- as.character( data[,2])
VMS$Latitude <- latitude
VMS$Longitude <- longitude
VMS$Date_Time <- data[,7]
VMS$Avg_Speed <- as.numeric(as.character(data[,8]))
VMS$Avg_Direction <- as.numeric(as.character(data[,9]))
VMS$Declarations <- as.numeric(as.character(data[,10])) # this should generate some NAs because some declarations are NA

#From here look to see if duplicates in a vessels time
Playboy <- VMS[which(VMS[,1]=="Playboy"),]

# do appear to be duplicates
which(duplicated(Playboy$Date_Time))

# plotting some of the duplicates
longs <- Playboy$Longitude[679:693]
lats <- Playboy$Latitude[679:693]

dz <- as.data.frame(cbind(longs,lats))
library(mapdata)
library(ggmaps)
ggplot(dz, aes(x = longs, y = lats)) + geom_point(color = "#00A0B0") + geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group)) + coord_map(xlim=c(-124.59,-124.45), ylim = c(42.735,42.750))

# looks like they're evenly spaced enough to consider them on that underlying grid. But definitely out at sea

ggplot(dz, aes(x = longs, y = lats)) + geom_point(color = "#00A0B0") + geom_polygon(data=bestcoast, aes(x = long, y = lat, group = group)) + coord_map(xlim=c(-124.57,-124.53), ylim = c(42.743,42.750))

# next step is to look at original log files to see if there are replicates there. can find the original file name by finding these records in the 'data' object

# using row name from 
Playboy[680:681,]

# gives this data line
data[336622:336623,]

# comes from VMS200901.log

# by using sed -n '1,$ { /Playboy/ { =; p; } }' VMS200901.log > wizard to find all the occurances of playboy and then sed -n '1,$ { /2009.01.03 21:44/ { =; p; } }' wizard > line_wizard to find all the occurances of one of the duplicated date_time s, find that there are all the duplicates.

# in conclusion, I did not introduce these into the data set. But now need to add in something to remove replicate entries.

# first find index of duplicate rows

diids <- which(duplicated(VMS))

