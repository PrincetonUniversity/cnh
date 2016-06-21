# generating example of how a type 3 harvesting function could generate multiple equilibrium. 

N = 0:K
q = .9
a = 23
p = 0.54
E = .9

type3 = p*q*E*N^2/(a^2+q*E*N^2)

plot(N,type3,type='l',lwd=2,col="red",frame=FALSE,ylab="Growth Rate/Predation Rate",xlim=c(0,K))

r = .012
K = 165
grow = r*N*(1-N/K)

lines(N,grow,lwd=2,col="blue")

q = 0.003
linear = q*E*N

lines(N,linear,lwd=2,col="grey")

legend("bottom",legend=c("Logistic growth","Type III Functional response","Linear (Type I) Functional Response"),lwd=2,col=c("blue","red","grey"),bty="n",cex=0.67)

