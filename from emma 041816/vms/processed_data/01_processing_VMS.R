# loading raw vms data ----
library(stringr)
# this runs on the VMS unzipped to my external hard-drive, because so much memory
# assumes working directory is CNH on Jameal's external drive
# make sure VMS_NMFS14-007 is unzipped
raw_vms <- dir("vms/raw_data/VMS_NMFS14-007")

# helper functions ----
# function to convert dms to decimal degrees
  dms_dec.deg <- function(df, col_num, lon_lat){
  # split on that degree symbol, save as dataframe
    list_data <- strsplit(as.character(df[,col_num]), "\\p{So}", perl=TRUE)
    sum_list <- summary(list_data)
    empties <- which(sum_list[,1]=="0")
    
    if(length(empties)>0){
      for(k in 1:length(empties)){
        list_data[[empties[k]]] <- c("0","0.0")
      }
    }
    
    dms <- as.data.frame(do.call(rbind, list_data), stringsAsFactors = FALSE)
  # split minutes/seconds into two columns
  dms[,3] <- as.numeric(dms[,2])-floor(as.numeric(dms[,2]))
  dms[,2] <- floor(as.numeric(dms[,2]))
  
  # convert to decimal degrees
  if(lon_lat == "lat"){
  dec <- as.numeric(dms[,1]) + (dms[,2] + dms[,3]/60)/60
  }else{
    if(lon_lat == "lon"){ # important because longitudes are negative for west coast
      dec <- as.numeric(dms[,1]) - (dms[,2] + dms[,3]/60)/60
    }
  }
  return(dec)
}

# processing each file ----
for(j in 1:length(raw_vms)){
  vms <- readLines(raw_vms[j],encoding="latin1")
  
# read fixed width lines one at a time ----
  list_rows <- vector("list", length(vms))
  
  for(i in 4:length(vms)){ # first four rows are headers
    list_rows[[i]] <- str_trim( # remove leading/trailing whitespace
      substring(vms[i], 
                first = c(1, 39, 60, 76, 92, 113, 124, 138),  
                last = c(38, 59, 75, 91, 112, 123, 137, 151))
    )
  }
  
  rm(vms) # save space
  
  # change to data.frame
  df <- as.data.frame(do.call(rbind, list_rows),stringsAsFactors=F)
  
  rm(list_rows) # save space
  
  # every 5000 rows, the header pops up again. find those rows and remove
  df <- df[-grep("---",df[,1]), ]
  df <- df[-grep("VESSEL_NAME",df[,1]), ]
  # there is still an empty row every 5000 (or 4998 now), will address when we convert a row to numeric
  
  # process each row
  # make character
  df[,2] <- as.character(df[,2])
  
# convert date-time to Etc/GMT-8 (to avoid problems with daylight savings) ----
  df[,5] <- as.POSIXct(df[,5], format = "%m/%d/%Y %H:%M", tz = "Etc/GMT-8")
  
# drop any NAs (gets rid of those empty rows) ----
  df <- df[complete.cases(df),]
  
# convert latitude to decimal degrees ----
  df[,3] <- dms_dec.deg(df, 3, lon_lat = "lat")
  # remove any longitude that is equal to zero
  if(any(df[,3]==0)) df <- df[-which(df[,3]==0),]
  
# convert longitude to decimal degrees ----
  df[,4] <- dms_dec.deg(df, 4, lon_lat = "lon")
  
# convert speed to numeric ----
  df[,6] <- as.numeric(df[,6])
  
# convert declarations to numeric ----
  df[,7] <- as.numeric(df[,7])
  
# rename column names ----
colnames(df) <- c("vessel.name", "doc.num","latitude","longitude","date.time","avg.speed","avg.direction","declarations")
# save file ----
saveRDS(df, paste0("../processed/df",j,".RDS"))

}

