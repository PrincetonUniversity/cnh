# trying to download Chl data and look at a single track of VMS data. 

install.packages("R.matlab")
require(R.matlab)

Chl.mat <- readMat("/Users/efuller/Desktop/MAT/2009/Data_0001.mat")
Chl.loc <- readMat("/Users/efuller/Desktop/MAT/Data_chl_locations.mat")

Chl.dat <- Chl.mat[[1]]
Chl.dat[is.nan(Chl.dat)] <- NA

#I remember now.. this is the east coast data. shoot. 

# now trying ROMS data
require(ncdf)
ROMS <- open.ncdf("/Users/efuller/Desktop/wc12_ccsra31_his_1day_avg_40531.nc")
Temp <- get.var.ncdf(ROMS,"temp")
