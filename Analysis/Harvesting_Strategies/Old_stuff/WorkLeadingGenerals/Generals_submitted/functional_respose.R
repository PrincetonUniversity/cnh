N = 0:50
r = 0.9
h = .3
b = 2

type1 = r*N
type2 = (r*N)/(1+r*h*N)
type3 = ((r*N)^b)/(1+r*h*N^b)

par(mfrow=c(1,3))
plot(N,type1,'l',lwd=2,frame=FALSE,xlab="",ylab="Predator's per capita consumption rate",axes=FALSE,cex.lab=1.4,col="red")
abline(h=0)
abline(v=0)
text(40,5,expression(C==rN),cex=1.4)
plot(N,type2,'l',lwd=2,frame=FALSE,xlab="Prey density",ylab="",axes=FALSE,cex.lab=1.4,col=colors()[148])
abline(h=0)
abline(v=0)
text(35,.5,expression(C==frac(rN,1+rhN)),cex=1.4)

N = 0:50
r = 0.9
h = .01
b = 2
type3 = ((r*N)^b)/(1+r*h*N^b)

plot(N,type3,'l',lwd=2,frame=FALSE,xlab=" ",ylab=" ",axes=FALSE,col=colors()[92])
text(35,15,expression(C==frac(rN^2,1+rhN^2)),cex=1.4)
abline(h=0)
abline(v=0)