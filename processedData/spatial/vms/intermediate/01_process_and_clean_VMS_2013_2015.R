# This script will clean the 2013-2015 VMS data received in May 2016

# Remove all garbage headers from each .txt file
# Ensure each column is formatted properly (date-time, etc)
# Spit out clean RDS and csv files to be used instead of raw .txt files


# Combine all clean RDS files into a single data frame
# Find on-land and non-West coast points and deal with them
# split the VMS tracks by vessel and process one by one

rm(list=ls())

library(stringr)

# load files
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")

# write a function to open each raw VMS data file and do bare bones cleaning.
# expect wanring messages about coercing NAs

#filename <- "VMSNWFS15-002_2013_01"

bare_bones <- function(filename){
  
  # Remove all garbage headers from each .txt file
  my_widths <- c(21,11,11,29,11,11,21,16)
  
  dat1 <- read.fwf(paste0("rawData/VMS/VMSdata_csv_NMFS16-002/",filename,".txt"),widths=my_widths,skip=32)
  #head(dat1,40)
 
  dat1a <- apply(dat1, 2, function (x) str_trim(x, side="both"))
  #head(dat1a,40)
  
  colnames(dat1a) <- c("DateTime", "Latitude", "Longitude", "VesselName", "AvgSpeed", "AvgCourse", "DocNum", "Declarations")
  dat1b <- data.frame(dat1a, stringsAsFactors = FALSE)
  #head(dat1b,40)
  
  # Ensure each column is formatted properly (date-time, etc)
  
  # format DateTime column
  dat1b$DateTime <- as.POSIXct(dat1b$DateTime, format = "%Y.%m.%d %H:%M", tz="Etc/GMT-8")
  #head(dat1b,40)
  
  # convert Lat-Long columnbs to numeric
  dat1b$Latitude <- as.numeric(dat1b$Latitude)
  dat1b$Longitude <- as.numeric(dat1b$Longitude)
  # head(which(is.na(dat1b$Latitude)))
  # dat1b[135:137,]
  # dat1b[235:237,]
  # dat1b[271:273,]
  
  # remove rows with NAs for Lat-Long (we think they occur when vessel names spillover)
  
  dat1c <- dat1b[-which(is.na(dat1b$Latitude)),]
  
  # head(dat1c,40)
  # head(which(is.na(dat1c$Longitude)))
  
  # make AvgSpeed and AvgCourse columns numeric
  dat1c$AvgSpeed <- as.numeric(dat1c$AvgSpeed)
  dat1c$AvgCourse <- as.numeric(dat1c$AvgCourse)
  
  # remove N/As from Declariations column
  dat1c$Declarations <- as.numeric(dat1c$Declarations)
  
  saveRDS(dat1c, paste0("rawData/VMS/VMSdata_csv_NMFS16-002_clean/",filename,"_clean.RDS"))
  write.csv(dat1c,paste0("rawData/VMS/VMSdata_csv_NMFS16-002_clean/",filename,"_clean.csv"), row.names=FALSE)
  
}

#system.time({bare_bones(filename)})

# read in filenames in directory
fn <-dir("rawData/VMS/VMSdata_csv_NMFS16-002")
fn2 <- gsub(".txt","",fn)

# create clean RDS and .csv files for all raw .txt files

for(i in 1:length(fn2)){
  
  bare_bones(fn2[i])
  
}

# Combine all clean RDS files into a single data frame

# read in data files

fn3 <-dir("rawData/VMS/VMSdata_csv_NMFS16-002_clean")
fn4 <- paste0("rawData/VMS/VMSdata_csv_NMFS16-002_clean/",fn3)
fn4 <- as.list(fn4)
data.list <- lapply(fn4, function(x) readRDS(x))

# paste all the data files together
df <- do.call(rbind,data.list)
vms2013_2015 <- df[-which(duplicated(df)),]
colnames(df) <- tolower(colnames(df))