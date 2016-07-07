# load files
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")

filename <- "VMSNWFS15-002_2013_01_clean"
d <- read.csv(paste0('rawData/VMS/VMSdata_csv_NMFS16-002_clean/', filename, '.csv'))
head(d)

i <-1
d1 <- c()
for(i in 1:dim(d)[1]){
d1 <- rbind(d1,diff(c(d$Longitude[i], d$Longitude[i+1]),na.rm=TRUE))
}
d1[1:10]              

min(d1[which(d1>0)])
