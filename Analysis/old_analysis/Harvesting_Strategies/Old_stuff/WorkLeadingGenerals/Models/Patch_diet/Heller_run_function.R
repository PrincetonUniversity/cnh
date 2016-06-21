rm(list=ls())

source("Heller_InPatch.R")

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
Tmax = seq(0,1000,100) # maximum time in patch
nrep = 10 # number of times to redo simulation 
rep_EG = 0
rep_n1 = 0
rep_n2 = 0
Mean_EG_G = 0
Var_EG_G = 0


for(i in 1:length(Tmax)){
  for(j in 1:nrep){
Results = Heller_InPatch(n1=n1,n2=n2,E1=E1,E2=E2,m1=m1,m2=m2,Q1=Q1,Q2=Q2,h1=h1,h2=h2,strategy="G",Tmax=Tmax[i])
rep_EG[j] = Results$EG[nrow(Results)] # final energy gotten in simulation j
rep_n1[j] = Results$n1[nrow(Results)] # final population of n1 in simulation j
rep_n2[j] = Results$n2[nrow(Results)] # final population of n2 in simulation j
  }
  Mean_EG_G[i] = mean(rep_EG)
  Var_EG_G[i] = sd(rep_EG)
  
}

plot(1:length(Mean_EG_G),Mean_EG_G,'l',lwd=2,xlab="Time in Patch",ylab="Net Energy Intake",col="blue")

Mean_EG_S1 = 0
Var_EG_S1 = 0

for(i in 1:length(Tmax)){
  for(j in 1:nrep){
    Results = Heller_InPatch(n1=n1,n2=n2,E1=E1,E2=E2,m1=m1,m2=m2,Q1=Q1,Q2=Q2,h1=h1,h2=h2,strategy="S1",Tmax=Tmax[i])
    rep_EG[j] = Results$EG[nrow(Results)] # final energy gotten in simulation j
    rep_n1[j] = Results$n1[nrow(Results)] # final population of n1 in simulation j
    rep_n2[j] = Results$n2[nrow(Results)] # final population of n2 in simulation j
  }
  Mean_EG_S1[i] = mean(rep_EG)
  Var_EG_S1[i] = sd(rep_EG)
}

lines(1:length(Mean_EG_S1),Mean_EG_S1,lwd=2,col="green")

Mean_EG_S2 = 0
Var_EG_S2 = 0

for(i in 1:length(Tmax)){
  for(j in 1:nrep){
    Results = Heller_InPatch(n1=n1,n2=n2,E1=E1,E2=E2,m1=m1,m2=m2,Q1=Q1,Q2=Q2,h1=h1,h2=h2,strategy="S2",Tmax=Tmax[i])
    rep_EG[j] = Results$EG[nrow(Results)] # final energy gotten in simulation j
    rep_n1[j] = Results$n1[nrow(Results)] # final population of n1 in simulation j
    rep_n2[j] = Results$n2[nrow(Results)] # final population of n2 in simulation j
  }
  Mean_EG_S2[i] = mean(rep_EG)
  Var_EG_S2[i] = sd(rep_EG)
}

lines(1:length(Mean_EG_S2),Mean_EG_S2,lwd=2,col="orange")

EG_all = cbind(1:length(Mean_EG_G),Mean_EG_G,Var_EG_G,Mean_EG_S1,Var_EG_S1,Mean_EG_S2,Var_EG_S2) 
EG_all = as.data.frame(EG_all)

G <- ggplot(EG_all)
G + geom_line(aes(x = V1,y=Mean_EG_G),color="blue")+geom_line(aes(x = V1,y=Mean_EG_S1),color="orange")+geom_line(aes(x = V1,y=Mean_EG_S2),color="green3")
G + geom_ribbon(aes(x= V1, y = Mean_EG_G, ymin=Mean_EG_G-Var_EG_G, ymax=Mean_EG_G+Var_EG_G),fill="lightblue") + geom_line(aes(x = V1,y=Mean_EG_G),color="blue") + geom_ribbon(aes(x = V1, y = Mean_EG_S1, ymin=Mean_EG_S1-Var_EG_S1,ymax = Mean_EG_S1+Var_EG_S1),fill="gold1") + geom_line(aes(x = V1,y=Mean_EG_S1),color="goldenrod4")+ geom_ribbon(aes(x = V1, y = Mean_EG_S1, ymin=Mean_EG_S1-Var_EG_S1,ymax = Mean_EG_S1+Var_EG_S1),fill="green1") + geom_line(aes(x = V1,y=Mean_EG_S1),color="green3")
