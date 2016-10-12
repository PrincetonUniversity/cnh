setwd("/Users/jamealsamhouri/Documents/cnh/")
setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/")

filename <- "J Samhouri - comp_ft _2010_2015"
dat1 <- read.csv(paste0("rawData/Catch/2010-2015_pacfin_data/",filename,".csv"))

colnames(dat1)
head(dat1)
