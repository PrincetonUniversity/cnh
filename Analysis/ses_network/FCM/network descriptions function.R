# num.species <- 14
# num.activities <- 6
# centrality.sp.id <- c(1,2)
# matrix.i <- all.dat[,,13]

# THIS VERSION OF THE FUNCTION FOCUSES EXCLUSIVELY ON THE SPECIES INTERACTION MATRIX, IGNORING ACTIVITIES
network.descriptions <- function(
  matrix.i,
  num.species,
  num.activities,
  centrality.sp.id
  ){
  
  matrix.i <- as.data.frame(matrix.i[c(1:num.species),c(1:num.species)]) 
  
  # set all self interactions = NA for now. 
  for(i in 1:length(matrix.i)){
    matrix.i[i,i]=NA
  }
  
  ### number of connections possible, N.possible
  # focus on just the species interaction matrix
  # for us, this is reduced because we are ignoring diagonal of matrix
  N.possible <- num.species*(num.species-1)
  
  ### number of connections
  C <- length(which(matrix.i != 0))
  
  ### C/N
  CtoN <- C/num.species
  
  ### Density of the FCMuyyui
  D <- C/N.possible
  
  ### Transmitter and Receiver calculations
  ### number of transmitters in FCM
  not.receiver <- which(colSums(abs(matrix.i),na.rm=TRUE) == 0)
  transmitter <- length(which(rowSums(abs(matrix.i[not.receiver,]),na.rm=TRUE) !=0))
  
  ### number of receivers in FCM
  # effect of row i on columns all =0, sum of effect of columns on row i !=0
  not.transmitter <- which(rowSums(abs(matrix.i),na.rm=TRUE) == 0)
  receiver <- length(which(colSums(abs(matrix.i[not.transmitter]),na.rm=TRUE) !=0))
  
  ### number of ordinary components in FCM
  ordinary <- length(which(rowSums(abs(matrix.i),na.rm=TRUE) !=0 & colSums(abs(matrix.i),na.rm=TRUE) != 0))
  
  ### complexity
  complexity <- ifelse(transmitter != 0, receiver/transmitter, 0)
  
  ####INTERACTION STRENGTH
  xbarwz <-   mean(abs(as.matrix(matrix.i)),na.rm=TRUE)
  xbarwnoz <- mean(abs(as.matrix(matrix.i[matrix.i!=0])),na.rm=TRUE)
    
  ### centrality of species in FCM
  centrality <- c()
  for(i in 1:length(centrality.sp.id)){
  centrality[i] <- sum(abs(matrix.i[,i]),na.rm=TRUE) + sum(abs(matrix.i[i,]),na.rm=TRUE)
  }
  names(centrality) <- paste("Centrality",my.headers[centrality.sp.id])
  
  ###HIERARCHY INDEX, h
  # from Hasiloglu 2009
  rescaled.matrix.i <- matrix.i/2
  outdegree.mean <- sum(rowSums(abs(rescaled.matrix.i),na.rm=TRUE))/num.species
  outdegree.variance <- sum((rowSums(abs(rescaled.matrix.i),na.rm=TRUE)-outdegree.mean)^2)/num.species
  h <- (12*outdegree.variance)/(num.species^2-1)
  
  
#   scalar <- 12/((num.species-1)*num.species*(num.species+1))
#   sum.rowsums <- sum(rowSums(abs(rescaled.matrix.i),na.rm=TRUE))
#   summed.quantity <- sum(((rowSums(abs(rescaled.matrix.i),na.rm=TRUE)-sum.rowsums)/num.species)^2)
#   scalar*summed.quantity
  
  # WRITE NETWORK DESRIPTION OUTPUT FILE
  OUT <- data.frame(Linkage.density=D, Connections=C, N_transmitters=transmitter, N_receivers=receiver, N_ordinary=ordinary, CtoN=CtoN, Complexity= complexity, Hierarchy_index=h,AveInt=xbarwz, AveIntNo0=xbarwnoz)
  OUT2 <- data.frame(c(OUT,centrality))
  return(OUT2)
}

#################################
#################################
#################################
#################################
# THIS VERSION OF THE FUNCTION FOCUSES FULL MATRIX, INCLUDING BOTH SPECIES AND ACTIVITIES
# network.descriptions <- function(
#   matrix.i,
#   num.species,
#   num.activities,
#   centrality.sp.id
# ){
#   
#   matrix.i <- as.data.frame(matrix.i) # assume matrix is formatted so 1st n rows are species, next m rows are activities
#   
#   # set all self interactions = NA for now. do the same for all species effects on activities.
#   for(i in 1:length(matrix.i)){
#     matrix.i[i,i]=NA
#     matrix.i[i,(num.species+1):(num.species+num.activities)]=NA
#   }
#   
#   ### number of connections possible, N.possible
#   # for us, this is reduced because we are ignoring diagonal of matrix
#   # and we did not ask about species effects on activities or activities effects on one another
#   N.possible <- num.species^2 - num.species + num.species*num.activities #266
#   
#   ### number of connections
#   C <- length(which(matrix.i != 0))
#   
#   ### Density of the FCM
#   D <- C/N.possible
#   
#   ### for Transmitter and Receiver calculations, can only focus on food web
#   food.web <- matrix.i[1:(nrow(matrix.i)-num.activities),1:(ncol(matrix.i)-num.activities)]
#   
#   ### number of transmitters in FCM (not including activities)
#   not.receiver <- which(colSums(abs(food.web),na.rm=TRUE) == 0)
#   transmitter <- length(which(rowSums(abs(food.web[not.receiver,]),na.rm=TRUE) !=0))
#   
#   ### number of receivers in FCM
#   # effect of row i on columns all =0, sum of effect of columns on row i !=0
#   not.transmitter <- which(rowSums(abs(food.web),na.rm=TRUE) == 0)
#   receiver <- length(which(colSums(abs(food.web[not.transmitter]),na.rm=TRUE) !=0))
#   
#   ### centrality of species in FCM
#   centrality <- c()
#   for(i in 1:length(centrality.sp.id)){
#     centrality[i] <- sum(abs(matrix.i[,i]),na.rm=TRUE) + sum(abs(matrix.i[i,]),na.rm=TRUE)
#   }
#   names(centrality) <- paste("Centrality",my.headers[centrality.sp.id])
#   
#   ############################
#   ### ADD HIERARCHY INDEX HERE
#   ############################
#   
#   # WRITE NETWORK DESRIPTION OUTPUT FILE
#   OUT <- data.frame(Linkage.density=D, N_transmitters=transmitter, N_receivers=receiver)
#   OUT2 <- data.frame(c(OUT,centrality))
#   return(OUT2)
# }