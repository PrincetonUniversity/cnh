# levy walk to simulate VMS track (from http://stackoverflow.com/questions/19208502/levy-walk-simulation-in-r)
alpha=2
n=1000
x=rep(0,n)
y=rep(0,n)

for (i in 2:n){
   theta=runif(1)*2*pi
   f=runif(1)^(-1/alpha)
   x[i]=x[i-1]+f*cos(theta)
   y[i]=y[i-1]+f*sin(theta)
}

# need to adjust so it will overlap with west coast

# mapping

expand <- c(-0,0)

xlim=range(x_lon)+expand
ylim=range(y_lat)+expand

# xlim <- c(-171.738281, -56.601563)
# ylim <- c(12.039321, 71.856229)
# map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)

plot(x_lon,y_lat,type="o",xlim=xlim,ylim=ylim,cex=0.5,col="white",bty="n",xaxt="n",yaxt="n",xlab="",ylab="")


map("worldHires","USA",xlim=xlim,ylim=ylim,fill=TRUE,col="grey", lwd=0.05,add=TRUE)

lines(x_lon,y_lat,type="o",xlim=xlim,ylim=ylim,cex=0.15,pch=19,col="black")


col_see <- sample(1:2,length(x_lon),replace=T)
col <- col_see
col[which(col==1)]="tomato"
col[which(col==2)]="turquoise"

points(x_lon,y_lat,xlim=xlim,ylim=ylim,cex=0.75,pch=19,col=col)