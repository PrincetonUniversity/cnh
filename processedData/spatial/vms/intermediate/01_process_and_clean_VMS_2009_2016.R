# This script will clean the Processed VMS csv files 2009-2016 received in Sep 2016
# also see blake's xlsx file "VMS csv processing summary"

# Remove all garbage headers from each .txt file
# Ensure each column is formatted properly (date-time, etc)
# Spit out clean RDS and csv files to be used instead of raw .txt files

library(stringr)

# load files
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")
setwd("/Users/jamealsamhouri/Documents/cnh/")

# write a function to open each raw VMS data file and do bare bones cleaning.
# expect wanring messages about coercing NAs

#filename <- "VMSNWFS16-007_2009" #

bare_bones <- function(filename){
#   
#   # Remove all garbage headers from each .txt file
#   
#   # read the number of dashes to determine how to break up columns
# #  dtemp <- read.table(paste0("rawData/VMS/Processed VMS csv files 2009-2016/",filename,".csv"), skip=1,nrow=1)
#   #my_widths <- as.vector(apply(dtemp,1,nchar)+1)
#   
#   #my_widths <- c(21,11,11,29,11,11,21,16)
#   
#   # determine how many rows to skip
#   x = 0
#   bad_dash = TRUE
#   while(bad_dash){
#     dtemp <- read.csv(paste0('rawData/VMS/Processed VMS csv files 2009-2016/', filename, '.csv'), skip = x, nrow = 1,
#                         stringsAsFactors = FALSE)
#     old_length = nchar(dtemp[1])
#     new_length = nchar(gsub('-',"",dtemp[1]))
#     bad_dash = ifelse(old_length!=new_length, FALSE, TRUE)
#     x = x + 1
#   }
#   
#   my_widths <- as.vector(apply(dtemp, 1, nchar)+1)
#   
#   dat1 <- read.fwf(paste0("rawData/VMS/Processed VMS csv files 2009-2016/",filename,".csv"),widths=my_widths,skip=x)
#   #head(dat1,40)
 
#  dat1a <- apply(dat1, 2, function (x) str_trim(x, side="both"))
  
  dat1a <- read.csv(paste0('rawData/VMS/Processed VMS csv files 2009-2016/', filename, '.csv'), stringsAsFactors = FALSE)
  #head(dat1a)
  
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
  
  saveRDS(dat1c, paste0("rawData/VMS/Processed VMS csv files 2009-2016_clean/",filename,"_clean.RDS"))
  write.csv(dat1c,paste0("rawData/VMS/Processed VMS csv files 2009-2016_clean/",filename,"_clean.csv"), row.names=FALSE)
  
}

#system.time({bare_bones(filename)})

# read in filenames in directory
fn <-dir("rawData/VMS/Processed VMS csv files 2009-2016")
fn2 <- gsub(".csv","",fn)

# create clean RDS and .csv files for all raw .txt files

for(i in 1:length(fn2)){
  
  bare_bones(fn2[i])
  
}

