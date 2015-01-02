#Short script for classifying behavioral states from movement data 


#Packages used: 
#class (knn method) 
#caret (creating the data partition) 

#helper function: parse data from file into floats
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

#######ABM Specific
#Read in data from Julia output
fishers <- read.csv('fishers.csv',sep=' ') 
speeds <- read.table('ss-speeds-1') 
angles <- read.table('ss-angles-1') 
MSSI <- read.table('ss-MSSI-1') 
#states <- read.table('states.csv')
#convert data to numeric floats
speeds <- as.numeric.factor(speeds[,])
MSSI <- as.numeric(MSSI[,])
angles <- as.numeric(angles[,])

#Copy states from the Julia output 
states <- fishers[,3]
stats <- data.frame(speeds, angles, MSSI)



#get vector for partitioning data
trainIndex <- createDataPartition(states, p = .2, list = FALSE, times = 1) 

#Partition stats matrix into training and testing sets
train <- stats[trainIndex,] 
test <- stats[-trainIndex,] 
cl <- states[trainIndex]


predict = knn(train, test,cl, k=19)

#Find the spots that are different between prediction and known states
diff <- as.numeric.factor(predict) - as.numeric(states[-trainIndex])
sum(diff == 0) / length(diff)






