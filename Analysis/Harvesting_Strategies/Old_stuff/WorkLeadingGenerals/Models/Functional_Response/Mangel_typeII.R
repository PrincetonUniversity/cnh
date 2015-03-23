## Mangel type II function

N = 0:100
q = 0.5
E = 10
cm = 1
C0 = 1


c = cm*q*E*N/(C0 + q*E*N)

plot(N,c,type='l',lwd=2,frame=FALSE)

cm = .7


c = cm*q*E*N/(C0 + q*E*N)

lines(N,c,lwd=2,col="red")