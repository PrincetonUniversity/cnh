######conduct press perturbations of coastwide participation network based on revenues

rm(list=ls())

library(ggplot2)
library(Hmisc)
library(reshape)
library(fpc)
library(vegan)

setwd("/Users/jameal.samhouri/Documents/CNH_to_github/cnh/Analysis/ses_network/output data")
num.prop.trips <- read.csv("Total number of trips and mean proportion of trips for top 10 metiers 2009-2013.csv", header=TRUE)

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM/code")

source('theme_acs.R')
source('multiplot.R')
source('relative change.R')
#source("compile matrices.R")
source("compile matrices (revenues).R")

colnames(temp.new2) <- c()
all.dat_sp <- temp.new2

#make empty array to store results from each network (matrix) for each scenario for each metier
n.scenarios <- 3
n.metiers <- length(my.headers)
ar <- array(dim = c(n.scenarios,n.metiers)) 
dimnames(ar) <- list(c("control","eliminate_POT1","decrease_POT1"),my.headers)
#dimnames(ar) <- list(c("control","eliminate_POT1"),my.headers)

#Hard Code Funciton ideas if time
##Add counter to record which matrix didn't equilibrate?
##add a matrix that records each respondents's max diffvec at stop 
##Add error if matrix has NAs
##hard code tolerance option  with default 10^-5
##Add squashing function: not everyone uses sigmoid

#step 1: determine steady state (SS) 
#step 2: multiply through different vectors for that SS to see outcome of perturbations
#step 3: compare the output of the different scenarios

####################################################################################################
######## Get a matrix of the control values 
####################################################################################################

# initialize matrix to store steady state relative abundance
em_c <- c()

converge <-matrix(0,ncol = 2,nrow=1) #check the convergence 
colnames(converge) <- c("diffvec","nits")

ts_length <- 1001
ts_ar <- matrix(nrow=ts_length,ncol=n.metiers) #make empty array
dimnames(ts_ar) <- list(c(seq(1:ts_length)),my.headers)


#for(r in 1:dim(all.dat_sp)[3]){  # no for loop needed with 1 network
  
  adj.m = all.dat_sp #[,,r] #pull out a single matrix 
  #dd <- mean(abs(adj.m),na.rm=TRUE)
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  #diag(adj.m) = rep(-dd,adj.m,length(diag(adj.m))) #insert density dependence
  #diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  adj.m [is.na(adj.m)==TRUE] <- 0 # convert NAs to zeroes
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  
  # start all metiers with the same relative abundance
  #emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  #ts_ar[1,] <- rep(1,ncol(adj.m))
  
  ########################
  ########################
  ########################
  # start all metiers with relative abundance based on proportion of trips they account for on average 2009-2013. 
  emat[1,] <- num.prop.trips$mean.prop.trips
  ts_ar[1,] <- num.prop.trips$mean.prop.trips
  #emat[1,] <- 1/(1+exp(-num.prop.trips$mean.prop.trips))
  #ts_ar[1,] <-1/(1+exp(-num.prop.trips$mean.prop.trips))
  # start all metiers with relative abundance based on number of trips they account for on average 2009-2013. 
  # emat[1,] <- num.prop.trips$num.trips.by.metier
  # ts_ar[1,] <- num.prop.trips$num.trips.by.metier
  ########################
  ########################
  ########################
  
  #vector for estimating differences as matrix converges
  diffvec <- rep(99, ncol(adj.m)) 
  
  #set the tolerance for when convergence is defined
  tol <- rep(10^-5,ncol(adj.m))
  
  #i=1
  for(i in 1:max.it){
    #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    tmpmat <- emat[i,] %*% adj.m 
    
    #scale 0 to 1 with logit transform
    #plot(seq(-1,1,0.01),1/(1+exp(-seq(-1,1,0.01))))
    #plot(seq(-10,10,0.01),1/(1+exp(-seq(-10,10,0.01))))
    emat[i+1,] <- 1/(1+exp(-tmpmat)) 
    
    ts_ar[i+1,] <- 1/(1+exp(-tmpmat)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge[1] <- max(diffvec)
      converge[2] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  
  em_c <- as.numeric(df1[nrow(df1),]) ##Save Equilibrium Value (i.e. last of df)
  em_c <- as.data.frame(cbind(my.headers,em_c))
  em_c[,2] <- as.numeric(as.character(em_c[,2]))
  colnames(em_c) <- c("Metier","Steady state relative abundance")
  setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM/output")
  #write.csv(em_c,"Steady state relative abundance for coastwide network based on trips.csv", row.names=FALSE)
  write.csv(em_c,"Steady state relative abundance for coastwide network based on revenues.csv", row.names=FALSE)
  
  ar[1,]<- as.numeric(df1[nrow(df1),])
  which(ar[1,]==min(ar[1,])) # trips: POT_1 was least abundant previously, Red sea urchin is least abundant on 4/5/16
  

### STOPPED HERE ON 1/29/16. NEXT STEP IS TO SIMULATE A CRAB CLOSURE AND ASK HOW PARTICIPATION (TRIPS) IS EXPECTED TO CHANGE RELATIVE TO THE STEADY STATE. 
  
### ALSO, DOES THE STEADY STATE EVEN MAKE SENSE???? URCHIN HAS LOWEST RELATIVE ABUNDANCE, SQUID AND SALMON GREATEST, RANGE IS QUITE SMALL 0.36-0.49.
  ### I THINK THE STEADY STATE IS AN INTERMEDIATE OUTPUT THAT IS USEFL FOR GAUGING THE EFFECTS OF PRESS PERTURBATIONS BUT NOT USEFUL UNTO ITSELF (1/30/16)
  
  colnames(df1) <- my.headers
  df1$time <- seq(1,nrow(df1),1)
  df_ts <- melt(df1,,id.vars="time")
  
  p1<- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
    geom_line()+
    geom_point(size=2)+
    xlim(0,nrow(df1))+
    xlab("Time")+ylab("Relative Abundance")+
    #ggtitle("Steady state analysis for top10 metiers \nbased on correlations among trips")+
    ggtitle("Steady state analysis for top10 metiers \nbased on correlations among revenues")+
    theme_bw() +
    theme(
      text=element_text(size=14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = NA,colour = "black",size=2)#,
      #legend.title = element_blank()
    )
  p1
  
  #png(filename = "Steady state top10 metiers based on correlations among trips coastwide.png",width = 800, height = 500)
  png(filename = "Steady state top10 metiers based on correlations among revenues coastwide.png",width = 800, height = 500)
  p1
  dev.off()
  # 
  
  #CRAB ELIMINATION
  ####################################################################################################
  ######## Get a matrix of the effects of crab elimination. subset to 9 node network. note change in tmpmat coding, inserts value each iteration
  ####################################################################################################
  
  em_crab_e <- c()
  
  converge_crab_e <-matrix(0,ncol = 2,nrow=1) #check the convergence 
  colnames(converge_crab_e) <- c("diffvec","nits")
  
  ts_length <- 1001
  ts_ar_crab_e <- matrix(nrow=ts_length,ncol=n.metiers-1) #make empty array
  dimnames(ts_ar_crab_e) <- list(c(seq(1:ts_length)),my.headers[-5])
  
#    for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m_crab_e = all.dat_sp[-5,-5] #[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m_crab_e) = rep(0,adj.m_crab_e,length(diag(adj.m_crab_e))) #insert no density dependence
  adj.m_crab_e [is.na(adj.m_crab_e)==TRUE] <- 0 # convert NAs to zeroes
  
  max.it_crab_e <- 1000 #set maximum iterations
  
  emat_crab_e <- matrix(nrow=(max.it_crab_e+1),ncol = ncol(adj.m_crab_e)) #make an empty matrix with species as columns
  emat_crab_e[1,] <- rep(1,ncol(adj.m_crab_e)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  #emat_crab_e[1,5] <- 0 #add the crab (POT_1) perturbation 
  
  # start all metiers with the same relative abundance
  ### OR BETTER TO START AT STEADY STATE ABUNDANCES? ###
  ts_ar_crab_e[1,] <- rep(1,ncol(adj.m_crab_e))
  
  #vector for estimating differences as matrix converges
  diffvec_crab_e <- rep(99, ncol(adj.m_crab_e)) 
  
  #set the tolerance for when convergence is defined
  tol_crab_e <- rep(10^-5,ncol(adj.m_crab_e))
  
  for(i in 1:max.it_crab_e){
      tmpmat1_e <- emat_crab_e[i,]
      #tmpmat1_e[5] <- 0 #eliminate crab (POT_1) 
      tmpmat2_e <- tmpmat1_e %*% adj.m_crab_e #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
      emat_crab_e[i+1,] <- 1/(1+exp(-tmpmat2_e)) #scale 0 to 1 with logit transform
      ts_ar_crab_e[i+1,] <- 1/(1+exp(-tmpmat2_e)) 
      
      if(i!=1){
        diffvec_crab_e <- abs(emat_crab_e[i,]-emat_crab_e[i-1,]) #subtract current row from prev row
        if(max(diffvec_crab_e) > min(tol_crab_e) & i<max.it_crab_e){ #if statement to stop when the tolerance is met
          print(paste("i=",i))
          print(paste("max(diffvec)=", max(diffvec_crab_e)))
        }
      }
      if(max(diffvec_crab_e) <= min(tol_crab_e) | i==max.it_crab_e){
        print(paste("Finished! max(diffvec)=", max(diffvec_crab_e)))
        converge_crab_e[1] <- max(diffvec_crab_e)
        converge_crab_e[2] <- i
        break
        #i=max.it
      }
    }  
    
    df1_crab_e <- data.frame(emat_crab_e) #make into data frame
    df1_crab_e <- subset(df1_crab_e,df1_crab_e[,1]!="NA") #subset NAs
    
    em_crab_e_raw <- as.numeric(df1_crab_e[nrow(df1_crab_e),]) ##Save Raw Equilibrium Value (i.e. last of df)
    em_crab_e_raw <- as.data.frame(cbind(my.headers[-5],em_crab_e_raw))
    em_crab_e_raw[,2] <- as.numeric(as.character(em_crab_e_raw[,2]))
    colnames(em_crab_e_raw) <- c("Metier","Eliminate crab (POT_1) steady state abundance")
    
    em_crab_e <- ((as.numeric(df1_crab_e[nrow(df1_crab_e),])-ar[1,-5])/ ar[1,-5])*100 ##Save Equilibrium Value (i.e. last of df) relative to steady state contained in ar[1,]
    em_crab_e <- as.data.frame(cbind(my.headers[-5],em_crab_e))
    em_crab_e[,2] <- as.numeric(as.character(em_crab_e[,2]))
    colnames(em_crab_e) <- c("Metier","Eliminate crab (POT_1) abundance relative to steady state with crab")
    
    df1_crab_e.tmp <- unlist(c(df1_crab_e[nrow(df1_crab_e),1:4],"NA",df1_crab_e[nrow(df1_crab_e),5:9]))
    # df1_crab_e.tmp <- as.data.frame(cbind(my.headers,df1_crab_e.tmp))
    # df1_crab_e.tmp[,2] <- as.numeric(as.character(df1_crab_e.tmp[,2]))
    # colnames(df1_crab_e.tmp) <- c("Metier","Eliminate crab (POT_1) steady state abundance")
    ar[2,]<- ((as.numeric(df1_crab_e.tmp)-ar[1,])/ ar[1,])*100
    
    setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM/output")
#     write.csv(em_crab_e_raw,"Eliminate crab (POT_1) steady state abundance for coastwide network based on trips.csv", row.names=FALSE)
#     write.csv(em_crab_e,"Eliminate crab (POT_1) change relative to steady state with crab for coastwide network based on trips.csv", row.names=FALSE)
    write.csv(em_crab_e_raw,"Eliminate crab (POT_1) steady state abundance for coastwide network based on revenues.csv", row.names=FALSE)
    #write.csv(em_crab_e,"Eliminate crab (POT_1) change relative to steady state with crab for coastwide network based on revenues.csv", row.names=FALSE)
    
    
    #write.csv(ar,"Steady state abundance and eliminate crab (POT_1) change relative to steady state with crab for coastwide network based on trips.csv", row.names=TRUE)

    
##########################################
##########################################
    
# correlation between interaction strengths and relative change

cor(all.dat_sp[,5],ar[2,], method="spearman",use="pairwise.complete.obs")
plot(all.dat_sp[,5],ar[2,])

df.cor.crab_e <- data.frame(my.headers,all.dat_sp[,5],as.numeric(unlist(c(em_crab_e[1:4,2],"NA",em_crab_e[5:9,2]))))
colnames(df.cor.crab_e) <- c("Metier", "Interaction.strength", "Rel.change.with.no.crab")

ggplot(df.cor.crab_e, aes(x=Interaction.strength, y=Rel.change.with.no.crab)) +
  geom_point(size=3)+
  theme_bw() +
  theme(
    text=element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2)#,
    #legend.title = element_blank()
  )
# ggsave("Association between metier interaction strengths (coastwide network based on trips) and eliminate crab (POT_1) change relative to steady state with crab 040516.pdf")

ggsave("Association between metier interaction strengths (coastwide network based on revenues) and eliminate crab (POT_1) change relative to steady state with crab 040516.pdf")

# strong negative correlation, indicating that the more negatively correlated trips were between a metier and POT_1, the more positive the expected effect of a crab closure. makes sense - BOOM

df.cor.crab_e$Metier.proper <- c("Sablefish",
                                "Red sea urchin",
                                "Market squid",
                                "Pacific sardine",
                                "Dungeness crab",
                                "CA spiny lobster",
                                "Chinook salmon",
                                "Albacore tuna",
                                "DTS",
                                "Pink shrimp")

my.order <- rev(sort(df.cor.crab_e$Metier.proper))
df.cor.crab_e$Metier.sort <- factor(df.cor.crab_e$Metier.proper, levels=my.order)
df.cor.crab_e.sort <- df.cor.crab_e [order(df.cor.crab_e$Metier.sort),]

ggplot(df.cor.crab_e.sort, aes(x=Metier.sort, y=Rel.change.with.no.crab)) +
  geom_bar(stat="identity")+
  coord_flip()+
  geom_hline(yintercept=0,linetype="dashed")+
  theme_bw() +
  #xlab("% Change With Crab Closure\n Based on Trips")+
  ylab("% Change With Crab Closure\nBased On Revenues")+
  #ylim(-2,12)+
  xlab("Fishery")+
  theme(
    text=element_text(size=18),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = NA,colour = "black",size=2)#,
    #legend.title = element_blank()
  )
#ggsave("Eliminate crab (POT_1) change relative to steady state with crab (coastwide network based on trips) 040516.pdf")
ggsave("Eliminate crab (POT_1) change relative to steady state with crab (coastwide network based on revenues) 040516.pdf")

##########################################
##########################################
    

#CRAB REDUCTION
####################################################################################################
######## Get a matrix of the effects of crab reduction. note change in tmpmat coding, inserts value each iteration
####################################################################################################

em_crabd <- c()

converge_crabd <-matrix(0,ncol = 2,nrow=1) #check the convergence 
colnames(converge_crabd) <- c("diffvec","nits")

ts_length <- 1001
ts_ar_crabd <- matrix(nrow=ts_length,ncol=n.metiers) #make empty array
dimnames(ts_ar_crabd) <- list(c(seq(1:ts_length)),my.headers)

#    for(r in 1:dim(all.dat_sp)[3]){  

adj.m_crabd = all.dat_sp #[,,r] #pull out a single matrix 
#diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
diag(adj.m_crabd) = rep(0,adj.m_crabd,length(diag(adj.m_crabd))) #insert no density dependence
adj.m_crabd [is.na(adj.m_crabd)==TRUE] <- 0 # convert NAs to zeroes

max.it_crabd <- 1000 #set maximum iterations

emat_crabd <- matrix(nrow=(max.it_crabd+1),ncol = ncol(adj.m_crabd)) #make an empty matrix with species as columns
emat_crabd[1,] <- rep(1,ncol(adj.m_crabd)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
emat_crabd[1,5] <- -1 #add the crab (POT_1) perturbation 

# start all metiers with the same relative abundance
### OR BETTER TO START AT STEADY STATE ABUNDANCES? ###
ts_ar_crabd[1,] <- rep(1,ncol(adj.m_crabd))

#vector for estimating differences as matrix converges
diffvec_crabd <- rep(99, ncol(adj.m_crabd)) 

#set the tolerance for when convergence is defined
tol_crabd <- rep(10^-5,ncol(adj.m_crabd))

for(i in 1:max.it_crabd){
  tmpmat1_d <- emat_crabd[i,]
  tmpmat1_d[5] <- 0 #reduce (clamp) crab (POT_1) 
  tmpmat2_d <- tmpmat1_d %*% adj.m_crabd #make a temporary matrix to multiply the adjacency vector with the interaction matrix
  emat_crabd[i+1,] <- 1/(1+exp(-tmpmat2_d)) #scale 0 to 1 with logit transform
  ts_ar_crabd[i+1,] <- 1/(1+exp(-tmpmat2_d)) 
  
  if(i!=1){
    diffvec_crabd <- abs(emat_crabd[i,]-emat_crabd[i-1,]) #subtract current row from prev row
    if(max(diffvec_crabd) > min(tol_crabd) & i<max.it_crabd){ #if statement to stop when the tolerance is met
      print(paste("i=",i))
      print(paste("max(diffvec)=", max(diffvec_crabd)))
    }
  }
  if(max(diffvec_crabd) <= min(tol_crabd) | i==max.it_crabd){
    print(paste("Finished! max(diffvec)=", max(diffvec_crabd)))
    converge_crabd[1] <- max(diffvec_crabd)
    converge_crabd[2] <- i
    break
    #i=max.it
  }
}  

df1_crabd <- data.frame(emat_crabd) #make into data frame
df1_crabd <- subset(df1_crabd,df1_crabd[,1]!="NA") #subset NAs

em_crabd_raw <- as.numeric(df1_crabd[nrow(df1_crabd),]) ##Save Raw Equilibrium Value (i.e. last of df)
em_crabd_raw <- as.data.frame(cbind(my.headers,em_crabd_raw))
em_crabd_raw[,2] <- as.numeric(as.character(em_crabd_raw[,2]))
colnames(em_crabd_raw) <- c("Metier","Decrease crab (POT_1) steady state abundance")

em_crabd <- ((as.numeric(df1_crabd[nrow(df1_crabd),])-ar[1,])/ ar[1,])*100 ##Save Equilibrium Value (i.e. last of df) relative to steady state contained in ar[1,]
em_crabd <- as.data.frame(cbind(my.headers,em_crabd))
em_crabd[,2] <- as.numeric(as.character(em_crabd[,2]))
colnames(em_crabd) <- c("Metier","Decrease crab (POT_1) abundance relative to steady state with crab")

df1_crabd.tmp <- unlist(c(df1_crabd[nrow(df1_crabd),]))
# df1_crabd.tmp <- as.data.frame(cbind(my.headers,df1_crabd.tmp))
# df1_crabd.tmp[,2] <- as.numeric(as.character(df1_crabd.tmp[,2]))
# colnames(df1_crabd.tmp) <- c("Metier","Eliminate crab (POT_1) steady state abundance")
ar[3,]<- ((as.numeric(df1_crabd.tmp)-ar[1,])/ ar[1,])*100

setwd("~/Documents/CNH_to_github/cnh/Analysis/ses_network/FCM/output")
#     write.csv(em_crabd_raw,"Eliminate crab (POT_1) steady state abundance for coastwide network based on trips.csv", row.names=FALSE)
#     write.csv(em_crabd,"Eliminate crab (POT_1) change relative to steady state with crab for coastwide network based on trips.csv", row.names=FALSE)
write.csv(em_crabd_raw,"Decrease crab (POT_1) steady state abundance for coastwide network based on revenues.csv", row.names=FALSE)
write.csv(em_crabd,"Decrease crab (POT_1) change relative to steady state with crab for coastwide network based on revenues.csv", row.names=FALSE)


write.csv(ar,"Steady state abundance, eliminate crab (POT_1), decrease crab (POT_1) change relative to steady state with crab for coastwide network based on trips.csv", row.names=TRUE)

### CONCLUSION: CLAMPING POT_1 AND SIMULATING DYNAMICS OF 9 NODE INTERACTION MATRIX LEAD TO SAME RESULT. REVISIT WITH EMMA

##########################################
##########################################



##########################################
##########################################
    
    
##########################################
##########################################
##########################################
##########################################
  
# cdf <- data.frame(converge)
# cdf$nits <-as.numeric(as.character(cdf$nits))
# subset(cdf,nits>999)[,1]
# dp <- which(cdf[,3]>999)
# 
# fu_ts <- ts_ar[-1,,dp]
# fu_ts <- melt(fu_ts)
# names(fu_ts) <- c("time","species","respondent","relative.abundance")
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#          geom_line(aes(colour=species))+
#          facet_wrap(~respondent,scales="free")+
#          theme_acs()


##Below we adapt the previous code to drop the people that don't converge re naming 'respondent.names'

#em_cc <- em_c[-c(dp),] #these are the equilibrium values that converge 
#respondent.names <- respondent.names[-dp]
#all.dat_sp <- all.dat_sp[,,-c(dp)] #cut out bc all converging now


#All Time Series 

ts <- melt(ts_ar[-1,,])
names(ts) <- c("time","species","respondent","relative.abundance")
ts <- subset(ts,relative.abundance!="NA")
gg_tsall = ggplot(ts,aes(x=time,y=relative.abundance))+
  geom_line(aes(colour=species))+
  facet_wrap(~respondent,scales="free")+
  theme_acs()

png(filename = "All Time Series.png",width = 1000, height = 1000)
gg_tsall
dev.off()

#example of each respondent
tshake <- subset(ts,respondent %in% c("Patrick O'Hara", "Dana Haggarty"))
tshake <- subset(tshake,species == "Pacific.Hake.Pacific.Cod.Sablefish")

gg_tshake = ggplot(tshake,aes(x=time,y=relative.abundance))+
  geom_line(aes(colour=species))+
  facet_wrap(~respondent)+
  theme_acs()

gg_tshake

subset(tshake,time==22)
dana.equilib <- subset(tshake,time==22)$relative.abundance[1]
patrick.equilib <- subset(tshake,time==22)$relative.abundance[2]

####################################################################################################
######## Get a matrix of the zooplankton increase: change in tmpmat coding, inserts value each iteration
####################################################################################################
em_z <- matrix(99,ncol=14,nrow=length(respondent.names))
colnames(em_z) <- names(temp.new)[1:14]
rownames(em_z) <- respondent.names

converge_z <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_z) <- c("respondentname","diffvec","nits")

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

  
for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,2] <- 1 #add the zooplankton perturbation (change from 1 to -1)
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    tmpmat1[2] <- 1 #increase zooplankton  
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_z[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_z[r,2] <- max(diffvec)
      converge_z[r,3] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_z[r,]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 ##Save Equilibrate Value (i.e. last of df)
  ar[2,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}


#example time series of last person in alphabet
names(df1) <- colnames(em_z)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_zts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_ZooplanktonIncrease_EG.png")


png(filename = "TS_ZooplanktonDecrease_EG.png",width = 800, height = 500)
em_zts
dev.off()


#All Time Series 

#All Time Series 

ts <- melt(ts_ar[-1,,])
names(ts) <- c("time","species","respondent","relative.abundance")
ts <- subset(ts,relative.abundance!="NA")
gg_tsall = ggplot(ts,aes(x=time,y=relative.abundance))+
  geom_line(aes(colour=species))+
  facet_wrap(~respondent,scales="free")+
  theme_acs()

png(filename = "All Time Series.png",width = 1000, height = 1000)
gg_tsall
dev.off()

#HERRING REDUCTION
####################################################################################################
######## Get a matrix of the Herring  Response note change in tmpmat coding, inserts value each iteration
####################################################################################################

em_h <- matrix(ncol=14,nrow=length(respondent.names))
colnames(em_h) <- names(temp.new)[1:14]
rownames(em_h) <- respondent.names

converge_h <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_h) <- c("respondentname","diffvec","nits")

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,1] <- -1 #add the herring perturbation 
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    tmpmat1[1] <- -1 #reduce herring 
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_h[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_h[r,2] <- max(diffvec)
      converge_h[r,3] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_h[r,]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 ##Save Equilibrate Value (i.e. last of df)
  ar[3,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}




###Check for and plot non converged time series
cdf <- data.frame(converge_h)
cdf$nits <-as.numeric(as.character(cdf$nits))
subset(cdf,nits>999)[,1]
dp <- which(cdf[,3]>999)
# 
# fu_ts <- ts_ar[-1,,dp]
# fu_ts <- melt(fu_ts)
# names(fu_ts) <- c("time","species","respondent","relative.abundance")
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#   geom_line(aes(colour=species))+
#   facet_wrap(~respondent)+
#   theme_acs()

#example time series of last person in alphabet
names(df1) <- colnames(em_c)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_hts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_HerringDecrease_EG.png")

png(filename = "TS_HerringDecrease_EG.png",width = 800, height = 500)
em_hts
dev.off()

########################################################
########################################Fix Scenario Below Here
########################################################


####################################################################################################
######## Get a matrix of the Pinniped Cull Response
####################################################################################################

em_p <- matrix(ncol=14,nrow=length(respondent.names))
colnames(em_p) <- names(temp.new)[1:14]
rownames(em_p) <- respondent.names


converge_p <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_p) <- c("respondentname","diffvec","nits")

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,12] <- -1 #add the pinniped perturbation (change from 1 to -1)
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    tmpmat1[12] <- -1 #reduce pinnipeds 
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_p[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_p[r,2] <- max(diffvec)
      converge_p[r,3] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_p[r,]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 ##Save Equilibrate Value (i.e. last of df)
  ar[4,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}

###Check for and plot non converged time series
cdf <- data.frame(converge_p)
cdf$nits <-as.numeric(as.character(cdf$nits))
subset(cdf,nits>999)[,1]
dp <- which(cdf[,3]>999)

# fu_ts <- ts_ar[,,dp]
# fu_ts <- melt(fu_ts)
# #names(fu_ts) <- c("time","species","respondent","relative.abundance") #only single person (Cam who isn't working) 
# names(fu_ts) <- c("time","species","relative.abundance")
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#   geom_line(aes(colour=species))+
# #  facet_wrap(~respondent)+
#   theme_acs()

#example time series of last person in alphabet
names(df1) <- colnames(em_c)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_pts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_PinnipedDecrease_EG.png")

png(filename = "TS_PinnipedDecrease_EG.png",width = 800, height = 500)
em_pts
dev.off()

####################################################################################################
######## Get a matrix of the Herring Increase
####################################################################################################

converge_hplus <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_hplus) <- c("respondentname","diffvec","nits")


em_hplus <- matrix(ncol=14,nrow=length(respondent.names))
colnames(em_hplus) <- names(temp.new)[1:14]
rownames(em_hplus) <- respondent.names

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,1] <- 1 #add the herring perturbation to starting values
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    tmpmat1[1] <- 1 
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_hplus[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_hplus[r,2] <- max(diffvec)
      converge_hplus[r,3] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_hplus[r,]<-((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 ##Save Equilibrate Value (i.e. last of df)
  ar[5,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}

#All Time Series 

ts <- melt(ts_ar[-1,,])
names(ts) <- c("time","species","respondent","relative.abundance")
ts <- subset(ts,relative.abundance!="NA")
gg_tsall = ggplot(ts,aes(x=time,y=relative.abundance))+
  geom_line(aes(colour=species))+
  facet_wrap(~respondent)+
  theme_acs()

png(filename = "All Time Series.png",width = 1000, height = 1000)
gg_tsall
dev.off()


#example of each respondent
tshake <- subset(ts,respondent %in% c("Patrick O'Hara", "Dana Haggarty"))
tshake <- subset(tshake,species == "Pacific.Hake.Pacific.Cod.Sablefish")

df <- data.frame(c(dana.equilib,patrick.equilib),
           c("dana","patrick"))

names(df) <- c("value","respondent")

gg_tshake = ggplot(tshake,aes(x=time,y=relative.abundance))+
  geom_line(aes(colour=respondent))+
  geom_hline(data = df,aes(yintercept = value,colour=respondent,),lty=2)+
  theme_acs()
  

gg_tshake

# ###Check for and plot non converged time series
# cdf <- data.frame(converge_hplus)
# cdf$nits <-as.numeric(as.character(cdf$nits))
# subset(cdf,nits>999)[,1]
# dp <- which(cdf[,3]>999)
# 
# fu_ts <- ts_ar[,,dp]
# fu_ts <- melt(fu_ts)
# #names(fu_ts) <- c("time","species","respondent","relative.abundance")
# names(fu_ts) <- c("time","species","relative.abundance")
# 
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#   geom_line(aes(colour=species))+
# #  facet_wrap(~respondent)+
#   theme_acs()

#example time series of last person in alphabet
names(df1) <- colnames(em_c)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_hplusts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_HerringIncrease_EG.png")


png(filename = "TS_HerringIncrease_EG.png",width = 800, height = 500)
em_hplusts
dev.off()


####################################################################################################
######## Get a matrix of the Whale increase
####################################################################################################

em_w <- matrix(ncol=14,nrow=length(respondent.names))
colnames(em_w) <- names(temp.new)[1:14]
rownames(em_w) <- respondent.names

converge_w <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_w) <- c("respondentname","diffvec","nits")

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,2] <- 1 #zooplankton increase
  emat[1,13] <- 1 #humpback whale increase
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    #tmpmat1[2] <- 1 #increase zoops 
    tmpmat1[13] <- 1 #increase humpbacks 
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_w[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_w[r,2] <- max(diffvec)
      converge_w[r,3] <- i
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_w[r,]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 ##Save Equilibrate Value (i.e. last of df)
  ar[6,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}

###Check for and plot non converged time series
# cdf <- data.frame(converge_w)
# cdf$nits <-as.numeric(as.character(cdf$nits))
# subset(cdf,nits>999)[,1]
# dp <- which(cdf[,3]>999)
# 
# fu_ts <- ts_ar[,,dp]
# fu_ts <- melt(fu_ts)
# names(fu_ts) <- c("time","species","respondent","relative.abundance")
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#   geom_line(aes(colour=species))+
#   facet_wrap(~respondent)+
#   theme_acs()


#example time series of last person in alphabet
names(df1) <- colnames(em_c)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_wts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_Whale_Increase_EG.png")

png(filename = "TS_WhaleANDZoopIncrease_EG.png",width = 800, height = 500)
em_wts
dev.off()



####################################################################################################
######## Get a matrix of the Whale increase Zoops increase
####################################################################################################

em_w_zneg <- matrix(ncol=14,nrow=length(respondent.names))
colnames(em_w_zneg) <- names(temp.new)[1:14]
rownames(em_w_zneg) <- respondent.names

converge_w_zneg <- matrix(0,ncol = 3,nrow=length(respondent.names))
colnames(converge_w_zneg) <- c("respondentname","diffvec","nits")

ts_ar <- array(dim = c(1001,14,length(respondent.names))) #make empty array
dimnames(ts_ar) <- list(c(seq(1:1001)),names(temp.new)[1:14],respondent.names)

for(r in 1:dim(all.dat_sp)[3]){  
  
  adj.m = all.dat_sp[,,r] #pull out a single matrix 
  #diag(adj.m) = rep(-1,adj.m,length(diag(adj.m))) #insert density dependence
  diag(adj.m) = rep(0,adj.m,length(diag(adj.m))) #insert no density dependence
  
  max.it <- 1000 #set maximum iterations
  
  emat <- matrix(nrow=(max.it+1),ncol = ncol(adj.m)) #make an empty matrix with species as columns
  emat[1,] <- rep(1,ncol(adj.m)) #define the adjacency matrix (i.e. the perturbation): 1 is no perturb. need to think about strength 
  emat[1,2] <- -1 #zooplankton decrease
  emat[1,13] <- 1 #humpback whale increase
  ts_ar[1,,] <- rep(1,ncol(adj.m))
  
  diffvec <- rep(99, ncol(adj.m)) #vctor for estimating differences as matrix converges
  tol <- rep(10^-5,ncol(adj.m)) #set the tolerance for when convergence is defined
  
  for(i in 1:max.it){
    tmpmat1 <- emat[i,]
    tmpmat1[2] <- -1 #decrease zoops 
    tmpmat1[13] <- 1 #increase humpbacks 
    tmpmat2 <- tmpmat1 %*% adj.m #make a temporary matrix to multiply the adjacency vector with the individual respondent's matrix
    emat[i+1,] <- 1/(1+exp(-tmpmat2)) #scale 0 to 1 with logit transform
    ts_ar[i+1,,r] <- 1/(1+exp(-tmpmat2)) 
    
    if(i!=1){
      diffvec <- abs(emat[i,]-emat[i-1,]) #subtract current row from prev row
      if(max(diffvec) > (10^-5) & i<max.it){ #if statement to stop when the tolerance is met
        print(paste("i=",i))
        print(paste("max(diffvec)=", max(diffvec)))
      }
    }
    if(max(diffvec) <= (10^-5) | i==max.it){
      print(paste("Finished! max(diffvec)=", max(diffvec)))
      converge_w_zneg[r,1] <- respondent.names[r] #pull out the names of the people 
      converge_w_zneg[r,2] <- max(diffvec)
      converge_w_zneg[r,3] <- i
      
      break
      #i=max.it
    }
  }  
  
  df1 <- data.frame(emat) #make into data frame
  df1 <- subset(df1,df1[,1]!="NA") #subset NAs
  em_w_zneg[r,]<-((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100  ##Save Equilibrate Value (i.e. last of df)
  ar[7,,r]<- ((as.numeric(df1[nrow(df1),])-ar[1,,r])/ ar[1,,r])*100 
}

# ###Check for and plot non converged time series
# cdf <- data.frame(converge_w_zneg)
# cdf$nits <-as.numeric(as.character(cdf$nits))
# subset(cdf,nits>999)[,1]
# dp <- which(cdf[,3]>999)
# 
# fu_ts <- ts_ar[,,dp]
# fu_ts <- melt(fu_ts)
# names(fu_ts) <- c("time","species","respondent","relative.abundance")
# fu_ts <- subset(fu_ts,relative.abundance!="NA")
# ggplot(fu_ts,aes(x=time,y=relative.abundance))+
#   geom_line(aes(colour=species))+
#   facet_wrap(~respondent)+
#   theme_acs()

#example time series of last person in alphabet
names(df1) <- colnames(em_c)
df1$time <- seq(1:length(df1[,1]))
df_ts <- melt(df1,id.vars=c("time"))

df_ts <-subset(df_ts,time!=1)
em_w_znegts <- ggplot(df_ts,aes(x=time,y=value,colour=variable))+
  geom_line()+
  geom_point(size=2)+
  theme_acs()+
  xlim(0,20)+
  xlab("Time")+ylab("Relative Abundance")+
  ggtitle("TS_WhaleIncreaseANDZoopDecrease_EG.png")

png(filename = "TS_WhaleIncreaseANDZoopDecrease_EG.png",width = 800, height = 500)
em_w_znegts
dev.off()


#multiplot(em_zts,em_hts,em_pts,em_hplusts,em_wts,em_w_znegts,cols=3)


####################################################################################################
######## Save output
####################################################################################################

# ##have a peak at output for just herring response 
# em_c[4,1]
# em_z[4,1]
# em_h[4,1]
# em_p[4,1]
# em_hplus[4,1]
# em_w[4,1]
# em_w_zneg[4,1]
# 
# setwd("/Users/adrianstier/Dropbox/Projects/NOAA Postdoc/Ocean Tipping Points/Research Activities/Case study - Haida Gwaii/Expert Survey/Analysis/R Code")
# save("em_c","em_z","em_h","em_p","em_hplus","em_w","em_w_zneg",file="ScenarioOutput_8_31.RData")




