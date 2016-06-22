
##### READ IN EACH MATRIX

# each matrix should have NAs specified on the diagonal
# include row and column names
# simulations will focus on effects of rows on columns

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM/input")

#temp <- read.csv("Correlations between metiers based on trips (symmetric matrix), adjusted years.csv", header=TRUE)
temp <- read.csv("Correlations between metiers based on revenues (symmetric matrix), adjusted years.csv", header=TRUE)

# may need to ensure that the matrix is the appropriate dimensions. blank was a dummy matrix we read in for this purpose: [1:nrow(blank),] 
num <- 10 # num species/metiers in analysis
temp.new <- temp[1:num,-1]

temp.new[1:3,1:3]

# convert the matrix into something R can perform calculations on: 

temp.new2 <- apply(temp.new,2,as.numeric)

# save the headers
my.headers <- names(temp.new)

###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###################################################
###############################
############# IDENTIFY RESPONDENT NAMES AND READ IN EACH MATRIX

# setwd("~/Dropbox/Expert Survey/Analysis/csv files")
# 
# 
# NAMES     <-  dir()
# FILE.NAME <-  "expert survey matrix.csv"
# LENG  	  <-	nchar(NAMES) # vector of length of file names
# MYSTART   <- nchar(NAMES) - nchar(FILE.NAME) # end of respondent name if a .csv file
# NAMES2    <-	NAMES[substr(NAMES,MYSTART+1,LENG)==FILE.NAME] # .csv files only
# 
# blank <- read.csv("blank matrix_AS.csv",header=TRUE)
# 
# # EACH RESPONDENT'S MATRIX IS SAVED IN THIS ARRAY
# all.dat = array(dim=c(nrow(blank),(ncol(blank)-1),length(NAMES2)))
# 
# respondent.names <- c() 
# 
# for(i in 1:length(NAMES2)){
#   temp <- read.csv(NAMES2[i],header=TRUE)[1:nrow(blank),] #read in Name[i]
#   #View(temp)
#   new.name		<-	substr(NAMES2[i],1,nchar(NAMES2[i])-nchar(FILE.NAME)-1) #subtract filename from person's name
#   respondent.names <- c(respondent.names,new.name) #tac on the next name
#   #substr(names(temp)[-1],1,5) == substr(temp[,1],1,5) # check to make sure rows and columns are in identical order
#   temp.new <- temp[,-1]
#   rownames(temp.new) <- names(temp.new)
#   #View(temp.new)
#   all.dat[,,i] <- apply(temp.new,2,as.numeric)
#   #temp.new2 <- temp.new[order(names(temp.new)),order(rownames(temp.new))]
#   #all.dat[,,i] <- apply(temp.new2,1,as.numeric)
# }
# 
# my.headers <- names(temp.new)
# 
# # jameal macbook
# #setwd("/Users/jameal.samhouri/Dropbox-NOAA/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Output_Input Files/July2014")
# # jameal iMac
# #setwd("/Users/jameal.samhouri/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Output_Input Files/July2014")
# #write.csv(respondent.names, "jameal respondent names.csv", row.names=FALSE)
# 
# ###################################################