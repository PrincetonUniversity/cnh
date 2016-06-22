### Load the package or install if not present
if (!require("RColorBrewer")) {
	install.packages("RColorBrewer")
	library(RColorBrewer)
}

if(!require("colorRamps")) {
	install.packages("colorRamps")
	library(colorRamps)
}

omega = 2000 # meters/hour
beta = 400 # meters
Tee = 100 # number of fishermen
alpha = 1/Tee + .2
h_1 = 1 # hour/g fish
h_2 = 1 # hour/g fish
q = .8 # catchability
r = .8 # growth rate of fish (1/time)
K = 100 # carrying capacity of fish (g fish)

N = seq(0,100,0.01)

fish_growth = r*N*(1-N/K)

Predation_rate = (alpha*omega*beta*q*Tee*N)/(1+omega*beta*q*(h_1+alpha*h_2)*N)

plot(N,fish_growth,type='l',lwd=2,frame=FALSE,col="chartreuse4")
lines(N,Predation_rate,lwd=2,col="goldenrod3")

## Plotting just the predation function. 

alpha = 1
Predation_rate = (alpha*omega*beta*q*Tee*N)/(1+omega*beta*q*(h_1+alpha*h_2)*N)
plot(N,Predation_rate,type="l",lwd=2)

# varying q does nothing
# varying omega does nothing
# varying alpha
alpha = seq(1/Tee,1,.01)
for(i in 1:length(alpha)){
Predation_rate = (alpha[i]*omega*beta*q*Tee*N)/(1+omega*beta*q*(h_1+alpha[i]*h_2)*N)
lines(N,Predation_rate,lwd=2,col=primary.colors(length(alpha))[i])
}

## Now imagining that it's type III
N = seq(0,.1,.0001)
c = .2
b = 0.001

alpha = 1
Predation_rate = (alpha*omega*beta*q*Tee*c*N^b)/(1+omega*beta*q*(h_1+alpha*h_2)*c*N^b)
plot(N,Predation_rate,type="l",lwd=2)

# varying alpha
alpha = seq(1/Tee,1,.01)
for(i in 1:length(alpha)){
Predation_rate = (alpha[i]*omega*beta*q*Tee*c*N^b)/(1+omega*beta*q*(h_1+alpha[i]*h_2)*c*N^b)
lines(N,Predation_rate,lwd=2,col=blue2red(length(alpha))[i])
}

## regular type III functional response
a = 2
H = .2
b = .6
c = .1
N = seq(0,10,.01)
type3 = a*c*N^b/(1+a*H*c*N^b)
plot(N,type3,type='l')

## current status, can't find the goddam type III functional response anywhere on the internet. and can't manage to get a sigmoidal response. 