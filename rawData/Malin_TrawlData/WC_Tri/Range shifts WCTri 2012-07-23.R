## How fast are species moving in West Coast Triennial data?


#########################
## Combine table ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NWFSC/WCTriSurveysAllCoast77thru04')
catch = read.csv('WCTri Access DB/CATCHWCTRIALLCOAST.csv')
	dim(catch)
	names(catch)
cruise = read.csv('WCTri Access DB/CRUISEWCTRIALLCOAST.csv')
	dim(cruise)
	names(cruise)
haul = read.csv('WCTri Access DB/HAULWCTRIALLCOAST.csv')
	dim(haul)
	names(haul)
species = read.csv('WCTri Access DB/RACEBASE_SPECIES.csv')
	dim(species)
	names(species)

# Add haul info to catch data, and make sure to add ALL hauls
data = merge(catch[,c('CRUISEJOIN', 'HAULJOIN', 'VESSEL', 'CRUISE', 'HAUL', 'SPECIES_CODE', 'WEIGHT', 'NUMBER_FISH')], haul[,c('CRUISEJOIN', 'HAULJOIN', 'VESSEL', 'CRUISE', 'HAUL', 'HAUL_TYPE', 'PERFORMANCE', 'START_TIME', 'DURATION', 'DISTANCE_FISHED', 'NET_WIDTH', 'NET_MEASURED', 'NET_HEIGHT', 'STRATUM', 'START_LATITUDE', 'END_LATITUDE', 'START_LONGITUDE', 'END_LONGITUDE', 'STATIONID', 'GEAR_DEPTH', 'BOTTOM_DEPTH', 'BOTTOM_TYPE', 'SURFACE_TEMPERATURE', 'GEAR_TEMPERATURE', 'GEAR', 'ACCESSORIES')], all.x=TRUE, all.y=TRUE)
	dim(data) # should be the same or greater nrows as catch: 108293
	dim(catch)

	# Turn NAs to 0 where hauls that were not in catch data were added
	i = which(is.na(data$SPECIES_CODE) & is.na(data$WEIGHT))
		length(i)
	data$SPECIES_CODE[i] = sort(unique(data$SPECIES_CODE))[1] # add a place-holder spp
	data$WEIGHT[i] = 0
	data$NUMBER_FISH[i] = 0


# Examine and then add species names
	sum(i <- duplicated(species$SPECIES_NAME)) # spp names are duplicated
		species$SPECIES_NAME[i] # all are blank scientific names
		species$COMMON_NAME[i] # all are vague categories of inverts
	sum(duplicated(species$SPECIES_CODE)) # ids are not duplicated

	data = merge(data, species[,c('SPECIES_CODE', 'SPECIES_NAME', 'COMMON_NAME')])
		dim(data) # should be the same or greater nrows as catch: 108293

# Add cruise data (only missing survey name right now)
	data = merge(data, cruise[,c('CRUISEJOIN', 'SURVEY_NAME')])
		dim(data) # 108293

# Create a unique haulid
	data$haulid = paste(formatC(data$VESSEL, width=3, flag=0), formatC(data$CRUISE, width=3, flag=0), formatC(data$HAUL, width=3, flag=0), sep='-')
	length(unique(data$haulid)) # 5614
	length(unique(data$HAULJOIN)) # 5614: great. should match number unique haulids	

# Trim to high-quality hauls
		table(data$HAUL_TYPE) # mostly 3: standard haul
	
	data = data[data$HAUL_TYPE==3,] # trim to standard hauls
		dim(data) # 107218
		
		table(-1 + (data$PERFORMANCE>=0) + (data$PERFORMANCE>0)) # negative: unsatisfactory, 0 is good, positive is satisfactory
	
	data = data[data$PERFORMANCE==0,] # trim to good performance
		dim(data) # 94345
		length(unique(data$haulid)) # 4551
		
# Extract year, month, day, time
	# Convert from local time to UTC
	dt = strptime(data$START_TIME, tz="America/Los_Angeles", format="%m/%d/%Y %H:%M:%S") # first convert to POSIXlt object
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
		# check my conversions
		data[!duplicated(data[,c('year', 'month', 'day')]),c('year', 'START_TIME', 'month', 'day', 'time')][1:10,]
	
	table(data$month, data$year)
		# was July-September in 1980-1989
		# was July-October in 1992
		# was June-August in 1995-2001
		# was June-July in 2004

	# Julian day
	require(date)
	data$julian = as.numeric(as.date(paste(data$month, '/', data$day, '/', data$year, sep='')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1

# Add strata (define by lat and depth bands)
	stratlatgrid = floor(data$START_LATITUDE)+0.5 # degree bins
	stratdepthgrid = floor(data$BOTTOM_DEPTH/100)*100 + 50 # 100 m bins
	data$stratum = paste(stratlatgrid, stratdepthgrid, sep='-')
		length(unique(data$stratum)) # 84

# Trim to high quality locations (sampled every year)
	table(data$STRATUM, data$year) # original strata are not consistent
	tab = table(data$stratum, data$year); tab # so use artificial strata
	sum = rowSums(tab>0)
	sumc = colSums(tab>0)
	as.data.frame(sum) # how many years per stratum?
	as.data.frame(sumc) # how many strata per year?
	
	rm = rownames(tab[sum<max(sum),])
	nrow(tab)
	length(rm)
	
	# trim to complete strata
	data = data[!(data$stratum %in% rm),]
		dim(data) # 71959	

	# Plot hauls by year, plus whether marked for removal
	quartz(width=12, height=7)
	par(mfrow=c(2,5), mai=c(0.5, 0.5, 0.2, 0.05), mgp=c(2,1,0))
	xlims = range(data$START_LONGITUDE)
	ylims = range(data$START_LATITUDE)
	yr = sort(unique(data$year))
	for(i in 1:length(yr)){
		 inds = data$year==yr[i] # all hauls each year
		inds2 = data$year==yr[i] & data$stratum %in% rm # hauls in strata to remove
		plot(data$START_LONGITUDE[inds], data$START_LATITUDE[inds], xlab='Longitude', ylab='Latitude', main=yr[i], cex=0.2, pch=16, xlim=xlims, ylim=ylims)
		points(data$START_LONGITUDE[inds2], data$START_LATITUDE[inds2], xlab='Longitude', ylab='Latitude', main=yr[i], cex=0.2, pch=16, xlim=xlims, ylim=ylims, col='red')
		abline(h=34:50, col='light grey')
	}
	
	# Examine by lat bins
#	latgrid = floor(data$START_LATITUDE)+0.5
#
#	i = !duplicated(data$HAULJOIN) # only unique hauls
#	table(latgrid[i], data$year[i]) # 36°N to 49°N sampled consistently
#
#	aggregate(list(lat=data$START_LATITUDE), by=list(year = data$year), FUN=range)
#
#	# Plot lat by year
#	yr = sort(unique(data$year[i]))
#	bks = seq(min(data$latgrid), max(data$latgrid), by=1)
#	par(mfrow=c(3, ceiling(length(yr)/3)))
#	for(j in 1:length(yr)){
#		hist(data$latgrid[i & data$year == yr[j]], breaks=bks, main=yr[j], xlab='Latitude')
#	}
#	
#	# Examine by long bins
#	longrid = floor(data$START_LONGITUDE)+0.5
#
#	i = !duplicated(data$HAULJOIN) # only unique hauls
#	table(longrid[i], data$year[i]) # 36°N to 49°N sampled consistently
#
#	aggregate(list(lon=data$START_LONGITUDE), by=list(year = data$year), FUN=range)
#
#	# Plot long by year
#	yr = sort(unique(data$year[i]))
#	bks = seq(min(data$longrid), max(data$longrid), by=1)
#	par(mfrow=c(3, ceiling(length(yr)/3)))
#	for(j in 1:length(yr)){
#		hist(data$longrid[i & data$year == yr[j]], breaks=bks, main=yr[j], xlab='Longitude')
#	}
#
#	# check ranges by year again
#	aggregate(list(lon=data$START_LONGITUDE), by=list(year = data$year), FUN=range)
#	aggregate(list(lat=data$START_LATITUDE), by=list(year = data$year), FUN=range)


# Reasonable depths? 
	#hist(data$BOTTOM_DEPTH) # 
	range(data$BOTTOM_DEPTH, na.rm=T) # 55-399

	# Examine by depth bins
	depthgrid = floor(data$BOTTOM_DEPTH/10)*10 + 5 # 10 m bins
		sort(unique(depthgrid))

	i = !duplicated(data$HAULJOIN) # only unique hauls
	table(depthgrid[i], data$year[i]) # 55-285 sampled consistently, except 1977. >220 sampled at low intensity each year.

	aggregate(list(lat=data$BOTTOM_DEPTH), by=list(year = data$year), FUN=range)


	# plot depth of sampling by lat by year
#	i = !duplicated(data$HAULJOIN) # only unique hauls
#	yr = sort(unique(data$year[i]))
#	bks = seq(min(data$depthgrid), max(data$depthgrid), by=10)
#	latbks = c(36, 39, 42, 45, 49)
#	quartz(width=6, height=8)
#	par(mfrow=c(length(yr), 4), mai=c(0.4,0.2,0.2,0.05), mgp=c(1.5, 0.7, 0))
#	for(j in 1:length(yr)){
#		for(k in 2:length(latbks)){
#			hist(data$depthgrid[i & data$year == yr[j] & data$latgrid > latbks[k-1] & data$latgrid < latbks[k]], breaks=bks, main=paste(yr[j], latbks[k-1], latbks[k]), xlab='Depth', col='grey', cex.main=0.8)
#		}
#	}


# Remove years that didn't sample all lat/depth bins?
	table(data$stratum, data$year) # check coverage again

# Fix column names
	names(data)[names(data)=='VESSEL'] = 'svvessel'
	names(data)[names(data)=='CRUISE'] = 'cruise'
	names(data)[names(data)=='STATIONID'] = 'station'
	names(data)[names(data)=='HAUL'] = 'tow'
	names(data)[names(data) == 'START_LATITUDE'] = 'lat'
	names(data)[names(data) == 'START_LONGITUDE'] = 'lon'
	names(data)[names(data) == 'GEAR_TEMPERATURE'] = 'bottemp'
	names(data)[names(data) == 'SURFACE_TEMPERATURE'] = 'surftemp'
	names(data)[names(data) == 'BOTTOM_DEPTH'] = 'depth'
	names(data)[names(data) == 'SPECIES_NAME'] = 'spp'
	names(data)[names(data) == 'COMMON_NAME'] = 'common'
	names(data)[names(data)=='WEIGHT'] = 'wtcpue'
	names(data)[names(data)=='NUMBER_FISH'] = 'numcpue'
	names(data)

# Check NAs and look for -9999
	summary(data$numcpue) # some NAs
	summary(data$wtcpue) # no NAs
	summary(data$surftemp) # some NAs (~ 1/20 of table)
	summary(data$bottemp) # many NAs (~ 1/3 of table)

# Add blank columns that are missing in this region
	data$surfsal = NA
	data$botsal = NA

# Add strata areas as the convex hulls for each stratum
# http://www.nceas.ucsb.edu/files/scicomp/GISSeminar/UseCases/CalculateConvexHull/CalculateConvexHullR.html
	require(PBSmapping)
	require(maptools)
	require(splancs)

	strats = sort(unique(data$stratum))
	strata = data.frame(stratum = strats, stratarea = numeric(length(strats)))
	for(i in 1:length(strats)){
		inds = which(data$stratum == strats[i])	
		hullpts = chull(x=data$lon[inds], y=data$lat[inds]) # find indices of vertices
		hullpts = c(hullpts,hullpts[1]) # close the loop
		ps = appendPolys(NULL,mat=as.matrix(data[inds,c('lon', 'lat')][hullpts,]),1,1,FALSE) # create a Polyset object
		attr(ps,"projection") = "LL" # set projection to lat/lon
		psUTM = convUL(ps, km=TRUE) # convert to UTM in km
		polygonArea = calcArea(psUTM,rollup=1)
		strata$stratarea[i] = polygonArea$area
		# plotPolys(ps); points(jitter(data$lon[inds]), jitter(data$lat[inds]))
	}
	strata	
	
	dim(data)
	data = merge(data, strata[,c('stratum', 'stratarea')])
	dim(data)


# Remove records without lat/long?
	i = is.na(data$lon) | is.na(data$lat)
	sum(i) # should be 0

# Check that gear and vessel were consistent within a year
	table(data$GEAR, data$year) # yes
	
	table(data$svvessel, data$year) # no, two vessels used each year
	
	# Plot hauls by vessel by year in a map (colors are not consistent between years)
#	quartz(width=12, height=7)
#	par(mfrow=c(2,5))
#	xlims = range(data$lat)
#	ylims = range(data$lon)
#	yr = sort(unique(data$year))
#	for(i in 1:length(yr)){
#		inds = data$year==yr[i] # all hauls each year
#		v = unique(data$svvessel[inds])
#		plot(data$lon[inds], data$lat[inds], xlab='Longitude', ylab='Latitude', main=yr[i], cex=0.2, pch=16, xlim=xlims, ylim=ylims, col=c('blue', 'green')[(data$svvessel[inds] == v[1])+1])
#	}
		
	
# Adjust for towed area
	summary(data$DISTANCE_FISHED) # 0.18 to 4.44 (km)
	summary(data$NET_WIDTH) # 9.5 to 18.06 (m?)
	unique(data$NET_MEASURED) # Y or N
	gear = unique(data$GEAR);gear # 172, 160, 162, or 170
		table(data$GEAR) # mostly 172 and 160
		table(data$GEAR, data$NET_MEASURED)

	# plot net width by gear
#	par(mfrow=c(2,2))
#	bks = seq(min(data$NET_WIDTH), max(data$NET_WIDTH), length.out=40)
#	for(i in 1:length(gear)){
#		inds = data$GEAR==gear[i]
#		hist(data$NET_WIDTH[inds], main=gear[i], breaks=bks, xlab='Net width (m)')
#	}
	
	data$numcpue = data$numcpue*10000/(data$DISTANCE_FISHED*1000*data$NET_WIDTH) # number per hectare (10,000 m2)	
	data$wtcpue = data$wtcpue*10000/(data$DISTANCE_FISHED*1000*data$NET_WIDTH) # number per hectare (10,000 m2)	

# Find duplicate tows in same location
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]
	
	# turn factors to chars so we can modify them
	data$svvessel = as.character(data$svvessel)
	data$cruise = as.character(data$cruise)
	data$tow = as.character(data$tow)
	data$stratum = as.character(data$stratum)
	data$spp = as.character(data$spp)
	data$common = as.character(data$common)

	# Any completely duplicated rows?
	dups = which(duplicated(data))
		sum(dups) # 0

	# find tows that have same lat/lon but different haulid
	inds = which(duplicated(data[,c('year', 'lat', 'lon')]) & !duplicated(data$haulid))
		length(inds) # 6

	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	idsdrops = numeric(0) # haulids to drop

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdata = data

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = c('svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	newdata = newdata[,nm]
		dim(newdata)

	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Find the best entry (least missing data or earliest) and delete the duplicate tows
	for(i in 1:length(inds)){
		if(i %% 10 == 0) print(i)
	
		inds2 = which(data$year == data$year[inds[i]] & data$lat == data$lat[inds[i]] & data$lon == data$lon[inds[i]])
		ids = sort(unique(data$haulid[inds2])) # find the haulids 
		data[inds2,]
			
		j0 = aggregate(list(surftemp = !is.na(data$surftemp[inds2]), bottemp = !is.na(data$bottemp[inds2])), by=list(haulid = data$haulid[inds2]), FUN=sum) # how many complete entries per haulid?
		j0$tot = (j0$surftemp>0) + (j0$bottemp>0) # how many complete variables per haulid?
		j0 = merge(j0, data[!duplicated(data$haulid) & data$haulid %in% ids, c('haulid', 'month', 'day', 'time')])
		j0 = j0[order(j0$month, j0$day, j0$time),] # order by time
		idskeep = j0$haulid[which(j0$tot == max(j0$tot))[1]] # pick the first haulid with the most complete entries
		idsdrop = ids[!(ids %in% idskeep)] # haulids to drop
		idsdrops = c(idsdrops, idsdrop)

		if(idskeep %in% idsdrops) stop('really, you want to keep this haulid? you wanted to drop it before') # make sure the haulid we like wasn't previously marked for removal
			
			# mark all rows in newdata with the other haulid for removal
		droprows = c(droprows, which(data$haulid %in% idsdrop))		
					
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values

	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdata$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows, and the tows I'm eliminating
	newdata = newdata[-droprows,]

	dim(data)
	dim(newdata)
	length(droprows) + nrow(newdata) # should match nrow(data)

# Create list of all hauls
	goodhauls = newdata[!duplicated(newdata$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhauls) # 3529
	head(goodhauls)

# Adjust spp names for those cases where they've changed
	table(newdata$spp, newdata$year)

	newdata$sppold = newdata$spp # save it for later checking for duplicate tows
	newdata$spp = as.character(newdata$spp)
	newdata$common = as.character(newdata$common)

	newdata$spp[newdata$sppold %in% c('Lepidopsetta polyxystra', 'Lepidopsetta bilineata')] = 'Lepidopsetta sp.'
	newdata$common[newdata$sppold %in% c('Lepidopsetta polyxystra', 'Lepidopsetta bilineata')] = 'rocksoleunident.'

	newdata$spp[newdata$sppold %in% c('Bathyraja interrupta', 'Bathyraja trachura', 'Bathyraja parmifera', 'Bathyraja spinosissima')] = 'Bathyrajasp.'
	newdata$common[newdata$sppold %in% c('Bathyraja interrupta', 'Bathyraja trachura', 'Bathyraja parmifera', 'Bathyraja spinosissima')] = '' # no common name

	# check changes: should be one name per search
	spp = unique(newdata$spp)
	spp[grep('Lepidopsetta', spp)]
	spp[grep('Bathyraja', spp)]

# Remove spp without a scientific name
	i = newdata$spp == ''
	sort(unique(newdata$common[i]))
	sum(i)

	newdata = newdata[!i,]
	dim(newdata) # 70858 rows
	length(unique(newdata$haulid)) # 3529
	nrow(goodhauls) # 3529 # lost no hauls by trimming out blank scientific names

# Find duplicate rows (repeat rows for the same tow), and combine entries from spp that have now been consolidated. Already checked for duplicate tows in same location (see above)
	# first sort by time so first index is always the earliest
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp),]

	dups = which(duplicated(newdata[, c('spp', 'year', 'lat', 'lon', 'haulid', 'wtcpue')])) # true duplicates, which would be a problem (should have been removed above)
	length(dups) # 0: not true duplicates

	inds = which(duplicated(newdata[, c('spp', 'year', 'haulid')])) # use the one with the most data
	length(inds) # 2

	# turn factors to chars so we can modify them
	newdata$station = as.character(newdata$station)
	newdata$common = as.character(newdata$common)
	newdata$spp = as.character(newdata$spp)
	newdata$time = as.character(newdata$time)

	# new copy of newdata so that averaging duplicate rows won't modify the original copy
	newdata2 = newdata

	# Trim some unneeded columns from newdata2
	nm = names(newdata)[!(names(newdata) %in% c('sppold'))]
	newdata2 = newdata2[,nm]
	names(newdata2)
	
	# Rows to drop
	droprows = numeric(0)
	
	# Sum the entries if they also match on tow, otherwise choose the best one
	# Sum the entries: looks like the right thing based on visual inspection of the data (same haulids)
	for(i in 1:length(inds)){
		if(i %% 100 == 0) print(i)
	
		inds2 = which(newdata$spp == newdata$spp[inds[i]] & newdata$year == newdata$year[inds[i]] & newdata$haulid == newdata$haulid[inds[i]])
		# inds2 = which(data$spp == data$spp[inds[i]] & data$year == data$year[inds[i]] & data$stratum == data$stratum[inds[i]] & data$station == data$station[inds[i]])
		data[inds2,]

		# put sums in the first row
		temp = data.frame(
		numcpue = sumna(newdata$numcpue[inds2]), 
		wtcpue = sumna(newdata$wtcpue[inds2]))

		newdata2$wtcpue[inds2[1]] = temp$wtcpue
		newdata2$numcpue[inds2[1]] = temp$numcpue

		# mark the following row(s) for removal
		droprows = c(droprows, inds2[2:length(inds2)])
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values
	length(droprows)
	
	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdata2$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows
	all(inds == droprows) # should be the same
	dim(newdata2)
	newdata2 = newdata2[-droprows,]
	dim(newdata2) # 70856

	dim(data)
	dim(newdata) # 70858
	length(inds)
	length(inds) + nrow(newdata2) # should match nrow(newdata)

			
# How many tows?
	length(unique(paste(newdata2$year, newdata2$lat, newdata2$lon, newdata2$stratum))) # 3529 unique locations
	length(unique(newdata2$haulid)) # 3529: matches: perfect

# How many spp?
	length(unique(newdata2$spp)) #538

# Any spp duplicated in the same haul?
	i = duplicated(paste(newdata2$haulid, newdata2$spp))
	sum(i) # 0
	
# Add a region column
newdata2$region = "WestCoast_Tri"

# How many hauls missing from data?
	setdiff(goodhauls$haulid, unique(newdata2$haulid)) # lost no hauls

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata2$month, newdata2$year) # surveys don't cross Jan 1, so yearsurv == year
	newdata2$yearsurv = newdata2$year
	newdata2$juliansurv = newdata2$julian	


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	ncol(newdata2)
	length(nm)
	setdiff(nm, names(newdata2))
	setdiff(names(newdata2), nm)

newdata2 = newdata2[,nm]
	dim(newdata2) # 70,856 x 26

# Write out
	write.csv(newdata2, paste('Output/data_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhauls, paste('Output/goodhauls_', Sys.Date(), '.csv', sep=''))

#########################################	
## Trim to spp with data and add zeros ##
#########################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NWFSC/WCTriSurveysAllCoast77thru04')

data = read.csv('Output/data_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
goodhauls = read.csv('Output/goodhauls_2012-04-02.csv', row.names=1)
	
# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(count=data$numcpue, weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(count=spplist$count, weight=spplist$weight, pres = spplist$pres>0), by=list(spp=spplist$spp), FUN=sum) # pres col holds # years in which spp was present

	spplist = spplist[order(spplist$count, decreasing=T),]
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 538 spp

	spplist = spplist[spplist$pres == max(spplist$pres),] # take all spp present >= 1x in every year
		nrow(spplist) # 74 spp	

	spplist = merge(spplist, data[!duplicated(data$spp), c('common', 'spp')])
		dim(spplist)
		head(spplist)

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 59593

# Any spp duplicated in the same haul?
	i = duplicated(paste(data$haulid, data$spp))
	sum(i)

	j = data$haulid == data$haulid[i] & data$spp == data$spp[i]
	sum(j)
	
	k = duplicated(data)
	sum(k) # the whole row is not duplicated. appears to only be different at wtcpue and numcpue

# Add any missing hauls
	inds = which(!(goodhauls$haulid %in% data$haulid))
	length(inds) # 0 none missing

# Fill in zeros
	fin = length(unique(data$haulid))*length(unique(data$spp)) # expected final size
		fin
	hauls = unique(data$haulid)
		length(hauls)
		nrow(goodhauls) # should match
		
	# set up sets of unique hauls and spp
	newdata = data[!duplicated(data$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'day')]
		dim(newdata)
		newdata = newdata[order(newdata$haulid),] # sort by haulid
		rownames(newdata) = 1:nrow(newdata)
	spps = data[!duplicated(data$spp), c('spp', 'common')]
		dim(spps)
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdata = newdata[rep(rownames(newdata), rep(nrow(spps), nrow(newdata))),]
		dim(newdata)
			
		# add spp info, replicated so that each haul gets a species
	newdata = cbind(newdata, spps[rep(rownames(spps), length(unique(newdata$haulid))),])
		dim(newdata)
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = data[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(data$numcpue)] = -9999
		datana$wtcpue[is.na(data$wtcpue)] = -9999
	newdata = merge(newdata, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdata)
		summary(newdata$numcpue)
		summary(newdata$wtcpue)

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdata$numcpue[is.na(newdata$numcpue)] = 0
	newdata$wtcpue[is.na(newdata$wtcpue)] = 0
	newdata$numcpue[newdata$numcpue == -9999] = NA
	newdata$wtcpue[newdata$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdata[,c('haulid', 'spp')]))
	length(inds) # YES, if this is > 0

	#data[inds,]
	
		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdata$haulid), by=list(spp=newdata$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdata$spp))
		length(inds) # should be 0
		#newdata[inds,]
		
		#newdata = newdata[-inds,]
		#dim(newdata)

	# sort by time so first index is always the earliest
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp),]
		dim(newdata) # 261146

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year) # surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

	newdata = newdata[,nm]
		dim(newdata) # 261,146 x 26

# Fix row names
	row.names(newdata) = 1:nrow(newdata)

# Write out
	write.csv(newdata, paste('Output/datatrimwzeros_', Sys.Date(), '.csv', sep=''))


#######################
### Climate Envelope ##
#######################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NWFSC/WCTriSurveysAllCoast77thru04')

## Read in data
load('../../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

data = read.csv('Output/datatrimwzeros_2012-07-23.csv', row.names=1)
	dim(data) # 261,146 x 26
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	tab = aggregate(data$wtcpue>0, by=list(spp=data$spp, year = data$year), FUN=sum, na.rm=T)
	tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
	tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
	sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
	sppnames # These are spp to consider using: 42
	
	# Trim data to right spp
	datatrim = data[data$spp %in% sppnames,]
	dim(datatrim) # 148,218 x 26
			
## Add HadISST data (min and max temp)
	datatrim$mintemp = NA
	datatrim$mintempmnth = NA
	datatrim$maxtemp = NA
	datatrim$maxtempmnth = NA

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	datatrim$latgrid = floor(datatrim$lat)+0.5
	datatrim$longrid = floor(datatrim$lon)+0.5
	inds = which(!duplicated(datatrim[,c('latgrid', 'longrid', 'year', 'month')]))
		length(inds) # 217 to fit
	for(i in 1:length(inds)){
		if(i %% 50 == 0) print(i)
		lat = as.character(datatrim$latgrid[inds[i]]) # to match hadisst grid
		long = as.character(datatrim$longrid[inds[i]]) # to match hadisst grid
			if(as.numeric(long) < -180) long = as.character(as.numeric(long)+360) # fix if long is west of date line
		yr = as.character(datatrim$year[inds[i]])
		lastyr = as.character(as.numeric(yr)-1)
		thesemons = as.character(1:datatrim$month[inds[i]]) # months in this year, converted to char
		lastmons = as.character((datatrim$month[inds[i]]+1):12) # months we want from last year

		j = datatrim$latgrid == datatrim$latgrid[inds[i]] & datatrim$longrid == datatrim$longrid[inds[i]] & datatrim$year == datatrim$year[inds[i]] & datatrim$month == datatrim$month[inds[i]]

		# since above are char, we can use them as indices into hadisst
		temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
		if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
			warning(paste('WARNING: No summer temps for i=', i))
		} else {
			datatrim$maxtemp[j] = max(temps, na.rm=T)
			datatrim$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
		}
		if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
			warning(paste('WARNING: No winter temps for i=', i))
		} else {
			datatrim$mintemp[j] = min(temps, na.rm=T)
			datatrim$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
		}
	}

# Add a date and a time column (rather than DATETIME)
datatrim$date = paste(formatC(datatrim$month, width=2, flag=0), '/', formatC(datatrim$day, width=2, flag=0), '/', datatrim$year, sep='')

# Transform data
	datatrim$lsurftemp = log(datatrim$surftemp+1)
	datatrim$lbottemp = log(datatrim$bottemp+1)
	datatrim$lmintemp = log(datatrim$mintemp+1)	
	
# Add pres/abs
	datatrim$pres = datatrim$numcpue>0 | (is.na(datatrim$numcpue) & !is.na(datatrim$wtcpue) & datatrim$wtcpue>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datatrim$wtcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm)
	dim(datatrim) # 148,218 x 38

bm = aggregate(list(nummean=datatrim$numcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm, all.x=T)
	dim(datatrim) # 148,218 x 39

# Have a cpue that never goes to zero (useful for fitting log-links)
datatrim$wtcpuena = datatrim$wtcpue
datatrim$wtcpuena[datatrim$wtcpuena == 0] = 1e-4
datatrim$wtcpuenal = log(datatrim$wtcpuena)

datatrim$numcpuena = datatrim$numcpue
datatrim$numcpuena[datatrim$numcpuena == 0] = 1e-4
datatrim$numcpuenal = log(datatrim$numcpuena)

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datatrim)
	length(nm)
	
	setdiff(nm, names(datatrim))
	setdiff(names(datatrim), nm) # remove stratarea and common
	 	 
datatrim = datatrim[,nm]
	dim(datatrim) # 148,218 x 41

## Write out for this species
	write.csv(datatrim, file=paste('Output/dataCEM_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NWFSC/WCTriSurveysAllCoast77thru04')

datatrim = read.csv('Output/dataCEM_2012-07-23.csv', row.names=1)
