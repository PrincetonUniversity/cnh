rm(list=ls())
setwd("~/Documents/Harvesting_Strategies/EmpiricalAnalysis/NOAA/Data_Analysis/Data/Malin_TrawlData/WC_Annual")
cruise <- read.csv('2003-2010HaulsOk.csv', colClasses=c("Trawl.Id"="character"))
	dim(cruise)
	names(cruise)

specimen <- read.csv('ComprehensiveDataPkg_BioSpecimen2003-2010OkHauls.csv',colClasses=c("TRAWL_ID"="character"))
	dim(specimen)
	names(specimen)
	
catch <- read.csv('2009-2010FishCatchOk.csv',colClasses=c("Trawl.Id"="character"))
	dim(catch)
	names(catch)
	
# Remove cruises before 2009
	cruise <- subset(cruise, Survey.Cycle == "Cycle 2009" | Survey.Cycle == "Cycle 2010")

# Add haul info to catch data, and make sure to ad all hauls (all.y = TRUE)
	data = merge(catch, cruise[,c('Survey.Cycle','Vessel', 'Trawl.Id', 'Trawl.Date', "Best.Latitude..dd.", 'Best.Longitude..dd.', "Best.Depth..m.", "Depth.Minimum..m.", "Depth.Maximum..m.", "Trawl.Duration..min.", "Area.Swept.by.the.Net..hectares.", "Temperature.At.the.Gear..degs.C.","Trawl.Start.Time")], all.y=TRUE, by = "Trawl.Id")

# Extract year, month, day, time 
	# Convert from local time to UTC
	dt = strptime(data$Trawl.Start.Time, tz="America/Los_Angeles", format="%m/%d/%Y %H:%M") # first convert to POSIXlt object
	head(dt)
	dt.pos = as.POSIXct(dt, tz='America/Los_Angeles') # convert to POSIXct object
		head(dt.pos)
	dtu = format(dt.pos, tz='GMT', usetz=TRUE) # convert to UTC in text
		head(dtu)
	dtl = as.POSIXlt(dtu, tz='GMT') # convert back to POSIXlt so I can extract year/month/day/time
		head(dtl)
	data$year = dtl$year + 1900 # month, add 1 since POSIXlt starts at 0
	data$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	data$day = dtl$mday # day of the month
	data$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')
# check conversions
	data[!duplicated(data[,c('year', 'month', 'day')]),c('year', 'Trawl.Start.Time', 'month', 'day', 'time')][1:10,]

# Julian day
	require(date)
	data$julian = as.numeric(as.date(paste(data$month, '/', data$day, '/', data$year, sep='')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1

# plot hauls by year
	quartz(width=12, height=7)
	par(mfrow=c(1,2), mai=c(0.5, 0.5, 0.2, 0.05), mgp=c(2,1,0))
	xlims = range(data$Best.Longitude..dd.)
	ylims = range(data$Best.Latitude..dd.)
	yr = sort(unique(data$year))
	
	for(i in 1:length(yr)){
			 inds = data$year==yr[i] # all hauls each year
			plot(data$Best.Longitude..dd.[inds], data$Best.Latitude..dd.[inds], xlab='Longitude', ylab='Latitude', main=yr[i], cex=0.2, pch=16, xlim=xlims, ylim=ylims)
			abline(h=34:50, col='light grey')
		}

# Reasonable depths?
	range(data$Depth.Maximum..m.,na.rm=T)	# 57.9-1281.8
	
	# Examin by depth bins
	depthgrid = floor(data$Depth.Maximum..m./10)*10 + 5 # 10 m bins
	sort(unique(depthgrid))
	
	i = !duplicated(data$Trawl.Id) # only unique hauls

	depths <- table(depthgrid[i], data$year[i]) # 55-285 sampled consistently, except 1977. >220 sampled at low intensity each year.

	barplot(depths[,2])

	aggregate(list(lat=data$Depth.Maximum..m.), by=list(year = data$year), FUN=range, na.rm=T)

# Fix column names
	names(data)[names(data) == 'Best.Latitude..dd.'] = 'lat'
	names(data)[names(data) == 'Best.Longitude..dd.'] = 'lon'
	names(data)[names(data) == 'Temperature.At.the.Gear..degs.C.'] = 'bottemp'
	names(data)[names(data) == 'Best.Depth..m.'] = 'depth'
	names(data)[names(data) == 'Species'] = 'spp'
	names(data)[names(data)=='Haul.Weight..kg.'] = 'weight'
	names(data)[names(data)=='Vessel'] = 'svvessel'


# Check NAs
	summary(data$wtcpue) # some NAs
	summary(data$bottemp) # many NAs 

# Remove records without lat/long?
	i = is.na(data$lon) | is.na(data$lat)
	sum(i) # should be 0

# Check that gear and vessel were consistent within a year
	table(data$svvessel, data$year) # no, two vessels used each year

# Plot hauls by vessel by year in a map (colors are not consistent between years)
	quartz(width=12, height=7)
	par(mfrow=c(1,2))
	ylims = range(data$lat)
	xlims = range(data$lon)
	yr = sort(unique(data$year))
	for(i in 1:length(yr)){
		inds = data$year==yr[i] # all hauls each year
		v = unique(data$svvessel[inds])
		plot(data$lon[inds], data$lat[inds], xlab='Longitude', ylab='Latitude', main=yr[i], cex=0.2, pch=16, xlim=xlims, ylim=ylims, col=c('blue', 'green')[(data$svvessel[inds] == v[1])+1])
	}

# adjust for towed area
	summary(data$Area.Swept.by.the.Net..hectares.)
	data$wtcpue <- data$weight/data$Area.Swept.by.the.Net..hectares.	# biomass per hectare

# Find duplicate tows in same location
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]

	# turn factors to chars so we can modify them
	data$svvessel = as.character(data$svvessel)
	data$spp = as.character(data$spp)
	
	# Any completely duplicated rows?
	dups = which(duplicated(data))
		sum(dups) # 0

	# find tows that have same lat/lon but different Trawl.Id
	inds = which(duplicated(data[,c('year', 'lat', 'lon')]) & !duplicated(data$haulid))
		length(inds) # 0

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
	nrow(spplist) # 306 spp

	spplist = spplist[spplist$pres == max(spplist$pres,na.rm=T),] # take all spp present in both years
		nrow(spplist) # 217 spp

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 59593

# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=data$Trawl.Id), by=list(spp=data$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls

write.csv(data,'Merged_data.csv')