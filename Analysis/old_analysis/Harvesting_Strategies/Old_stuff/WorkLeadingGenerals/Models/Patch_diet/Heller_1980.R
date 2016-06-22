rm(list = ls())
require("ggplot2")

EG = 0    # cumulative net energy intake, starts at zero
E1 = 2.5
E2 = 0.5
h1 = 7
h2 = 5
Q1 = 0.35
Q2 = 0.85
m1 = 0.17
m2 = 0.05
n1 = 1    # number of prey species 1 to start
n2 = 1    # number of prey species 2 to start
n1_max = n1 # maximum number of n1 ever
n2_max = n2 # maximum number of n2 ever
tac = "S1"  # tactic, could be specialist on species 1, S1; specialist on species 2, S2; generalist, G; expanding generalist, SG
tp_max = 10000 # maximum time
T = 0   # initializing time


while(T<tp_max){
print(T)
  if(n1[length(n1)] > 0) {            # are there prey 1 left?, if yes
    
    encounter_1 = Q1*(n1_max-m1*n1[length(n1)]) # probability of encountering prey 1
    
    if(runif(1,0,162)<encounter_1 && tac %in% c("G","S1")){    # if encounter, your tactic must also be generalist or specialist on prey 1
        EG[length(EG)+1] = EG[length(EG)]+E1/h1 # add to your total energy
        T = T + h1        # spend h1 on handling prey
        n1[length(n1)+1] = n1[length(n1)] - m1       # n1 reduced by m1
        n2[length(n2)+1] = n2[length(n2)] # no n2 eaten this loop
        next              # start the loop again
      } 
  }
  
  if(n2[length(n2)] > 0) {           # are there prey 2 left?, if yes
    
    encounter_2 = Q2*(n2_max-m2*n2[length(n2)]) # probability of encountering prey 2
    
    if(runif(1,0,162)<encounter_2 && tac %in% c("G","S2")){  # if encounter, your tactic must be generalist or specialist on 2
      EG[length(EG)+1] = EG[length(EG)] + E2/h2  # add to your total energy
      T = T + h2         # spend h2 on handling prey
      n2[length(n2)+1] = n2[length(n2)] - m2        # reduce n2 population by m2
      n1[length(n1)+1] = n1[length(n1)]    # no n1 eaten this round
      next               # start loop over again
    } else {
      T = T+1            # didn't find any n1 or n2, add one metric of time and start again
      EG[length(EG)+1] = EG[length(EG)]
      n2[length(n2)+1] = n2[length(n2)]
      n1[length(n1)+1] = n1[length(n1)]
    }
  }
  T = ifelse(n1[length(n1)] <= 0 && n2[length(n2)] <=0, T + 1,T) # time still goes to end if no prey left. 
  T = ifelse(n2[length(n2)] <= 0 && tac %in% "S2",T + 1, T)      # if tactic specializes on species 2 and there's none left, go through time
    }

par(mfrow=c(2,1))
plot(1:length(EG),EG,'l',lwd = 2,frame=FALSE,xlab="Time",ylab="Net Energy Intake",col="red")
plot(1:length(n2),n2,'l',lwd = 2,frame=FALSE,xlab="Time",ylab = "Population Size",ylim=c(0,1),col="green")
lines(1:length(n1),n1,'l',lwd=2,col="orange")
legend('bottomleft',legend=c("species 1","speces 2"),lwd=2,col=c("orange","green"),bty="n")

#qplot(1:length(EG),EG,geom=c("line"),xlab="Time",ylab="Energy Gotten",aes("red"))