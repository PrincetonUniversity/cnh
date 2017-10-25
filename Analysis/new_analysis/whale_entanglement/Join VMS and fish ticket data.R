library(foreign)
library(dplyr)
library(tidyr)
library(lubridate) #https://rstudio-pubs-static.s3.amazonaws.com/28038_1bcb9aa80ca84f27ace07d612872861a.html

data.dir <-"~/Documents/Github/cnh/processedData/both/fromBlakeOct2017"
setwd(data.dir)

#######################################
### READ IN AND PREP DATA ###
#######################################

#2009-2016 FISH TICKET DATA PRE-PROCESSED FROM BLAKE WITH TARGET COLUMN
  # the .csv is copied directly from "2009-2016 fish tickets processing VMS only V4.xlsx"
  # includes only vessels monitored by VMS

fishtix <- read.csv("fish tickets 2009-2016.csv", header=TRUE)
head(fishtix)
colnames(fishtix)[1] <- "Rec_ID"

fishtix$date <- parse_date_time(fishtix$date, orders = "mdy")

#######################################

#######################################
### MATCH FISH TIX TO VMS ###
#######################################


# make a list of VMS files and count them
  # set working directory
  mypath <- data.dir
  setwd(mypath)

  # identify dbf files. 
  mylist <- list.files(mypath,pattern = "geo.dbf") ######################### USE UPDATED FILES WITH BATHYMETRY
  ### ignore whale grid only file for now ###
  mylist <- mylist[1]
  
  num_vms_years <- length(mylist)

# make a list of vessels and count them
vessel_list <- as.character(unique(fishtix$drvid))
num_vessels <- length(vessel_list)

# make a dummy output dataframe
vms_out <- c()
tStart <- Sys.time()

# while for loop y is off
# tmp_dbf <- tmp_dbfdata
# while for loop i is off
#i=784 

######################### PROBABLY DON'T WANT TO USE Y LOOP, TOO MUCH MEMORY

for (y in 1:num_vms_years){

  # read in vms dbf y
  tmp_dbf_name <- mylist[y]
  tmp_dbf <- read.dbf(tmp_dbf_name, as.is = TRUE)

  # fix VMS data date-time
  tmp_dbf$UTCDATETIM <- parse_date_time(tmp_dbf$UTCDATETIM, orders = "ymd HM")
  
  # identify year for this VMS data set
  tmp_vms_year <- unique(year(tmp_dbf$UTCDATETIM))
  tmp_vms_year_begin <- paste0(tmp_vms_year,"-01-01")
  tmp_vms_year_end <- paste0(tmp_vms_year,"-12-31")
  
  # truncate fishtix to match VMS year
  fishtix_VMSyear <- 
    fishtix %>%
    filter(date >= tmp_vms_year_begin & date <= tmp_vms_year_end)
  
  # select a vessel to focus on
  for(i in 1:num_vessels){
    
    cat("Starting vessel", i, "\n")
    
    # choose a vessel to work with
    tmp_vessel <- vessel_list[i]
  
    # determine if this vessel has VMS data in the same year as the current VMS file
    tmp_num_vms_records <- length(which(tmp_dbf$DOCNUM == tmp_vessel))
    
    # determine if this vessel has fish tix in the same year as the current VMS file
    tmp_num_fishtix <- length(which(fishtix_VMSyear$drvid == tmp_vessel))
    
    # if there are not VMS records or fishtix for this vessel in this year skip, otherwise continue
    if( tmp_num_vms_records > 0 & tmp_num_fishtix > 0) {
      
      # subset to VMS records for focal vessel
      tmp_dbf_for_tmp_vessel <- tmp_dbf[which(tmp_dbf$DOCNUM == tmp_vessel),]
      
      # subset to fishtix records for focal vessel
      fishtix_VMSyear_for_tmp_vessel <- fishtix_VMSyear[which(fishtix_VMSyear$drvid == tmp_vessel),]
      
      # determine number of fishtix in the VMS year for this vessel
      numfishtix <- length(which(fishtix_VMSyear$drvid == tmp_vessel))
      
      # looping over focal vessel fishtix for this year
      for(j in 1:numfishtix){
        
        # sort fish tix for focal vessel from most recent to least recent
        fishtix_VMSyear_for_tmp_vessel_ordered <- arrange(fishtix_VMSyear_for_tmp_vessel, desc(date))
            
        # identify which fish ticket to consider most recent ("end") and the one previous to the most recent ("begin)
        end <- fishtix_VMSyear_for_tmp_vessel_ordered$date[j]
        begin <- fishtix_VMSyear_for_tmp_vessel_ordered$date[j+1]
        
        ######################### CONSIDER CHANGING BEGIN TO A MIN/MAX STATEMENT WITH A PRE-SPECIFIED WINDOW TO SEARCH BACK
        
        # create a new VMS dataframe that has a new column relating each geolocation to target group (feistier)
        vmstmp <-
          tmp_dbf_for_tmp_vessel %>%
          filter(UTCDATETIM >= begin & UTCDATETIM <= end) %>%
          mutate(
            TARGET = as.character(fishtix_VMSyear_for_tmp_vessel_ordered[j,'TARGET'])
          )
        
        vms_out <- rbind(vms_out, vmstmp)
    
      } # end for loop j from 1:numfishtix
    } # end if statement for tmp_num_vms_records
  } # end for loop i for 1:num_vessels

  print(Sys.time() - tStart)
  
}# end vms loop y from 1:num_vms_years

#############################################
#############################################

# add a column to the .dbf output that fish ticket and target group to define unique trips

#############################################
#############################################

setwd("~/Dropbox/Projects/In progress/Feist et al. whale entanglement/Output")

write.csv(vms_out,"VMS 2010 bath region with Target Feistier.csv", row.names = FALSE)

######################### JOIN OUTPUT TO ORIGINAL DBF FILE

vms_out2 <- vms_out
vms_out2$UTCDATETIM <- as.character(vms_out2$UTCDATETIM)
write.dbf(vms_out2,"VMS 2010 bath region with Target Feistier.dbf")





########### GRAVEYARD

#VMS DATA FROM GIS

# dbfdata <- read.dbf("whale_grid_only_2010_geo.dbf", as.is = TRUE)
# head(dbfdata)

# dum <- dbfdata$UTCDATETIM[1:5]
# parse_date_time(dum, orders = "ymd HM")
# dbfdata$UTCDATETIM <- parse_date_time(dbfdata$UTCDATETIM, orders = "ymd HM")

# START WITH TOY EXAMPLE FOR ONE VESSEL

# choose a vessel with largest landings for any one fish ticket
drvid_maxdailyDCRB <- as.character(fishtix$drvid[which(fishtix$DCRB....==max(fishtix$DCRB....))])
length(which(fishtix$drvid == drvid_maxdailyDCRB)) #531

# how many VMS geolocations for this vessel in 2010?
length(which(dbfdata$DOCNUM == drvid_maxdailyDCRB)) #4115
# are any of this vessel's fishtix from 2010?
head(fishtix[which(fishtix$drvid == drvid_maxdailyDCRB),],20)
length(which(fishtix$date[which(fishtix$drvid == drvid_maxdailyDCRB)] < "2011-01-01" &  
               fishtix$date[which(fishtix$drvid == drvid_maxdailyDCRB)] >= "2010-01-01")) # 74 fish tickets in 2010

## use this vessel to try matching fish tix

tmp_fishtix <- fishtix[which(fishtix$drvid == drvid_maxdailyDCRB & 
                               fishtix$date >= "2010-01-01" & 
                               fishtix$date <= "2010-12-31"),]
dim(tmp_fishtix)
head(tmp_fishtix)

tmp_dbfdata <- dbfdata[which(dbfdata$DOCNUM == drvid_maxdailyDCRB),]
dim(tmp_dbfdata)
head(tmp_dbfdata)

tmp_fishtix_ordered <- arrange(tmp_fishtix, desc(date))
head(tmp_fishtix_ordered)



# vessel with most DCRB over 2009-2016
# drvid_maxcumulativeDCRB <- as.character(unique(fishtix$drvid[which(fishtix$Dung_Tot==max(fishtix$Dung_Tot))]))
# length(which(fishtix$drvid == drvid_maxcumulativeDCRB)) #251
# length(which(dbfdata$DOCNUM == drvid_maxcumulativeDCRB)) #4564

# WORK THROUGH EACH DATA SET BY DRVID
# 1) SUBSET FISH TIX TO DRVID, FIND 2 MOST RECENT

vessel_list <- unique(fishtix$drvid)
i=1
length(
  which(dbfdata$DOCNUM == vessel_list[i])
) #467
length(
  which(fishtix$drvid == vessel_list[i])
) #220

vmstmp <- dbfdata[which(dbfdata$DOCNUM == vessel_list[i]),]
tixtmp <- fishtix[which(fishtix$drvid == vessel_list[i]),]

begin <- parse_date_time("2010-03-22","ymd")
end <- parse_date_time("2010-03-25","ymd")
tixtmp2 <- tixtmp %>%
  filter( (date >= begin & date <= end ) ) 
vmstmp2 <- vmstmp %>%
  filter( (UTCDATETIM >= begin & UTCDATETIM <= end ) )

window <- 7
vmstmp2 %>%
  left_join(tixtmp2, by = c("DOCNUM"="drvid")) %>%
  filter(UTCDATETIM <= date-window) %>%
  select(TARGET)

# https://stackoverflow.com/questions/45625496/merge-data-frames-by-time-interval-in-r
# eyedata  %>%
#   left_join(trials, by = c("subid", "condition")) %>%
#   filter( (time >= begin & time <= end)) 
