####################################
####################################

# THIS FILE USES GRAPH THEORY TO DESCRIBE NETWORKS OF INDIVIDUAL RESPONDENTS
# METRICS ARE DERIVED FROM GRAY ET AL. BOOK CHAPTER, OZESMI AND OZESMI 2004

####################################
####################################

# jameal iMac
#setwd("/Users/jameal.samhouri/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Code")
# jameal macbook
#setwd("/Users/jameal.samhouri/Dropbox-NOAA/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Code")
#stier macbook
setwd("~/Dropbox/Projects/In review/Expert Survey/Analysis/R Code")

source("compile expert surveys 070814.R")

# jameal iMac
#setwd("/Users/jameal.samhouri/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Code")
# jameal macbook
#setwd("/Users/jameal.samhouri/Dropbox-NOAA/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Code")

setwd("~/Dropbox/Projects/In review/Expert Survey/Analysis/R Code")


source("network descriptions function.R")
n.species <- 14
n.activities <- 6

#rescale data like we did for the scenario analysis
all.dat <- all.dat/2

######### MODIFY THIS DEPENDING WHICH SPECIES YOU'D LIKE TO KNOW ABOUT IN TERMS OF CENTRALITY. YOU CAN CHECK WHICH SPECIES EACH NUMBER CORRESPONDS TO BY QUERYING my.headers
centrality.sp <- c(1,2,5,12,13)

# run function on a single respondent matrix
#network.descriptions(all.dat[,,2],n.species,n.activities,centrality.sp)

# run function for all respondent matrices
out <- c()
for(j in 1:length(respondent.names)){
  nd <- network.descriptions(all.dat[,,j],n.species,n.activities,centrality.sp)
  tmp <- data.frame(Respondent.name=respondent.names[j],nd)
  out <- rbind(out,tmp)
}

# jameal iMac
#setwd("/Users/jameal.samhouri/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Output_Input Files/July2014")
# jameal macbook
#setwd("/Users/jameal.samhouri/Dropbox-NOAA/Dropbox/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Output_Input Files/July2014")
#adrian macbook
setwd("~/Dropbox/Projects/In review/Expert Survey/Analysis/R Output_Input Files/July2014")

write.csv(out, "Network descriptions for individual respondents.csv",row.names=FALSE)
