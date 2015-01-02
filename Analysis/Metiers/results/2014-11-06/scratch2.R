# trying bigmemory package
library(bigmemory)
setwd("/Users/efuller/1/CNH/Analysis/Metiers/results/2014-11-09/")

# need to build a distance-like matrix using Bray-Curtis as the measure of similarity

# > 30,000 is too big for a pairwise matrix with bigmemory. So the entire dataset is > 100,000. And a single year is ~ 90,000 so can't do an entire year either. 
# it's possible instead to use this just to speed up existing code by splitting on existing gear groups and trip years. 

library(permute); library(reshape2); library(plyr); library(dplyr); library(vegan); library(igraph)

# for practice, let's try defining 2010 hkl

# remove any double gear trips
ftl <- readRDS("../../writing/code/1_cleaningData/filtered_ftl.RDS")
just_gear <- select(ftl, ftid, grid)
just_gear <- unique(just_gear)
gear_types <- table(just_gear$ftid, just_gear$grid)
total_gear <- rowSums(gear_types)
extra_gear_trips <- names(total_gear)[which(total_gear > 1)]

ftl_trips <- subset(ftl, !(ftid %in% extra_gear_trips))

year = 2010
tickets = ftl_trips
gear_group = "HKL"

port_trips <- tickets[tickets[["year"]] == year & tickets[["grgroup"]] == gear_group, ]
port_trips <- subset(port_trips, !(veid %in% c("0","UNKNOWN","********")))
melt_trips <- melt(port_trips, id.vars = c("veid","ftid","modified","tdate","grid"), measure.vars = "landed_wt")
cast_trips <- dcast(melt_trips, ftid ~ modified, fun.aggregate = sum)
rownames(cast_trips) <- cast_trips$ftid
cast_trips$ftid <- NULL

# now system.time the regular way to do this. 
startime <- Sys.time()
bc <- vegdist(cast_trips, method = "bray")
bc_sim <- abs(bc-1)
bc_mat <- as.matrix(bc_sim)
elapsed <- ((Sys.time()-startime))

startime <- Sys.time()
bc <- vegdist(cast_trips, method = "bray",upper=TRUE)
bc_sim <- abs(bc-1)
bc_mat <- as.matrix(bc_sim)
elapsed1 <- ((Sys.time()-startime))

# trying by hand with filebacked matrix
x = cast_trips[1,]; y = cast_trips[2,]
bc_man <- sum( pmin(x,y)) / (sum(x) + sum(y))
bc_vegan <- vegdist(rbind(x,y), "bray")
# don't really get why this is.. ah! they have to be in common.. so pmin will return non-zero values when one has 0 and the other has some number. 
bc_other <- sum(abs(x-y))/sum(x+y)
bc_other==bc_vegan
# but this is it..
bc_mat <- filebacked.big.matrix(nrow=nrow(cast_trips), ncol=nrow(cast_trips), type = "short", init = NA, backingfile = "bc_mat.bin", descriptorfile = "bc_mat.desc", dimnames=list(rownames(cast_trips), rownames(cast_trips)))

# has a hard time with even 17,000. Doesn't bode well. But maybe on della is better? anyway let's see what "short" data means
startime <- Sys.time()
for(i in 1:nrow(cast_trips)){
  for(j in 1:nrow(cast_trips)){
    bc_mat[i,j] <- 1 - (sum(abs(cast_trips[i,] - cast_trips[j,]))/sum(cast_trips[i,]+cast_trips[j,]))
  }
}
print(Sys.time()-startime)

load("trip_mat.Rdata")
trip_mat <- bar
rm(bar)
class(trip_mat)
rownames(trip_mat) <- trip_mat$X
trip_mat$X <- NULL

trips_09 <- trip_mat[grepl("2009", rownames(trip_mat)),]

D <- filebacked.big.matrix(nrow = nrow(trips_09), ncol = nrow(trips_09),  
                            backingfile = "trip_mat.bin",
                           descriptorfile = "trip_mat.desc",
                           dimnames = list(
                             rownames(trips_09),rownames(trips_09)))


nrow(trip_mat)

createDistanceMatrix <- function(Inventory,Directory = getwd(), filename= “distance.bin”){
  require(bigmemory)
  
  deg2rad <- function(deg) return(deg*pi/180)
  
  if(!grepl(“\\.bin”,filename))stop(“please use a bin extension”)
  columnsPresent <- intersect(colnames(Inventory),c(“Id”,”Lon”,”Lat”))
  if(length(columnsPresent) != 3) stop(“missing the correct column names”)
  
  descName <- sub(“bin”,”desc”,filename)
  if(class(Inventory) == “matrix”) Inventory <- as.data.frame(Inventory)
  if(length(unique(Inventory$Id)) != length(Inventory$Id))stop(“Ids, must be unique”)
  # some temporary variables to speed up processing of distance calculation
  # these are done once and resused inside the loop
  Inventory[, "sinLat"] <- sin(deg2rad(Inventory$Lat))
  Inventory[, "cosLat"] <- cos(deg2rad(Inventory$Lat))
  Inventory$Lon <- deg2rad(Inventory$Lon)
  ER <- 6371
  
  nr <- nrow(Inventory) 
  nc <- nrow(Inventory)
  
  options(bigmemory.allow.dimnames=TRUE)
  
  D <- filebacked.big.matrix(nrow = nr, ncol = nc,
                             dimnames = list(as.list(Inventory$Id),NULL),
                             init = NA,
                             backingpath = Directory, 
                             backingfile = filename, 
                             descriptorfile = descName, 
                             type = “double”)
  
  validLonLat <- which(!is.na(Inventory$Lat) & !is.na(Inventory$Lon))
  
  for(i in validLonLat[-length(validLonLat)]){
    for(j in validLonLat[validLonLat > i] ){
      
      D[i,j] <- acos(Inventory$sinLat[i] * Inventory$sinLat[j]
                     + Inventory$cosLat[i] * Inventory$cosLat[j]
                     * cos(Inventory$Lon[j]-Inventory$Lon[i])) * ER
      if(is.nan(D[i,j])) D[i,j] <- 0
      
    }
  }
  
  return(D)
  
}

getNeighbors <- function(descriptFile = “distance.desc”,Id){
  require(bigmemory)
  
  DATA <- attach.big.matrix(dget(descriptFile))
  
  if(class(Id) != “character”){
    warning(“coercing Id to character”)
    Id <- as.character(Id)
  }
  dex <- which(rownames(DATA) == Id)
  
  if(length(dex) == 0)stop(“index not found”)
  if(length(dex) > 1)stop(“index found multiple times”) 
  
  neighbors <- c(DATA[1:dex,dex],DATA[dex,(dex+1):ncol(DATA)])
  neighbors[dex] <- 0
  names(neighbors) <- rownames(DATA)
  return(neighbors)
}


  