# have processor ID, looking at landing receipts to see if I can identify trips based on amount and date. Also connecting to see which from VMS I have. Not great. 

# look for small fish processor ----
ftl <- readRDS("/Users/efuller/1/CNH/processedData/catch/1_cleaningData/tickets.RDS")
class(ftl$processorid)
unique(ftl$processorid[grep("60764",ftl$processorid)])

unique(ftl$drvid[grep("60764",ftl$processorid)])

# looking for any of the boats in VMS data ----
vms <- readRDS("/Users/efuller/1/CNH/processedData/spatial/vms/intermediate/04_with_trip/VMS_catch.RDS")

found_boats <- subset(vms, doc.num %in% unique(vms$doc.num)[which(unique(vms$doc.num) %in% unique(ftl$drvid[grep("60764",ftl$processorid)]))])

with(found_boats, plot(longitude, latitude,asp=1, col = as.numeric(factor(doc.num)),pch=3, cex = .5))
library(maps)
map('state',add=T)

# see if I can find any of the landings dates recorded in Alan's records
subset(ftl, processorid=="6076401" & year == 2012 & modified == "DCRB" & month == 1 & day < 18)
