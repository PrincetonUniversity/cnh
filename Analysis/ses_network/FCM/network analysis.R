####################################
####################################

# THIS FILE USES GRAPH THEORY TO DESCRIBE STRUCTURAL PROPERTIES OF NETWORKS 
# METRICS ARE DERIVED FROM GRAY ET AL. BOOK CHAPTER, OZESMI AND OZESMI 2004

####################################
####################################

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM")
source("compile matrices.R")
source("network descriptions function.R")

n.metiers <- length(my.headers)
num.activities <- 0

# may need to rescale data if interaction strengths are too large

######### MODIFY THIS DEPENDING WHICH SPECIES YOU'D LIKE TO KNOW ABOUT IN TERMS OF CENTRALITY. YOU CAN CHECK WHICH SPECIES EACH NUMBER CORRESPONDS TO BY QUERYING my.headers
centrality.sp <- c(5)

# run function on a single  matrix
out <- network.descriptions(temp.new2,n.metiers,n.activities,centrality.sp)

write.csv(out, "Network descriptions for individual respondents.csv",row.names=FALSE)


# run function for multiple matrices
# out <- c()
# for(j in 1:length(respondent.names)){
#   nd <- network.descriptions(all.dat[,,j],n.species,n.activities,centrality.sp)
#   tmp <- data.frame(Respondent.name=respondent.names[j],nd)
#   out <- rbind(out,tmp)
# }

