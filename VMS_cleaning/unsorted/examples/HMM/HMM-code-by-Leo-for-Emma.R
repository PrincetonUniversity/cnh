#rm(list=ls())

library(proj4)  # For project
library(MASS)  # For fitdistr
library(mvtnorm)  # Required by mhsmm
library(mhsmm)  # For hmmfit
library(boot)  # Required by CircStats
library(CircStats)  # For rose.diag
library(multicore)  # For mclapply

#Some utility functions
latlong.distance <- function(lat, long){
	#Uses haversine formula for great circle distance; input is degrees, returns distance in meters
	lat.1 <- lat[1:(length(lat)-1)]*pi/180
	lat.2 <- lat[2:length(lat)]*pi/180
	long.1 <- long[1:(length(long)-1)]*pi/180
	long.2 <- long[2:length(long)]*pi/180
	delta.long <- (long[2:length(long)]-long[1:(length(long)-1)])*pi/180
	delta.sig <- atan((sqrt((cos(lat.2)*sin(delta.long))^2+
		(cos(lat.1)*sin(lat.2)-sin(lat.1)*cos(lat.2)*cos(delta.long))^2))/(sin(lat.1)*sin(lat.2)+
		cos(lat.1)*cos(lat.2)*cos(delta.long)))
	return(6371*delta.sig*1000)
}

abs.direction.function <- function(x1,x2,y1,y2){
	#Returns the "global" direction from (x1,y1) to (x2,y2) with east at zero degrees, north at 90, south at -90, and west at -180
	(x1<x2&y1<y2)*(atan((y2-y1)/(x2-x1)))+
	(x2<x1&y1<y2)*(pi+atan((y2-y1)/(x2-x1)))+
	(x2<x1&y2<y1)*(-pi+atan((y2-y1)/(x2-x1)))+
	(x1<x2&y2<y1)*(-atan((y2-y1)/(x1-x2)))+
	(x1<x2&y1==y2)*(0)+
	(x2<x1&y1==y2)*(pi)+
	(x1==x2&y1>y2)*(-pi/2)+
	(x1==x2&y1<y2)*(pi/2)	
}

turning.angle.function <- function(theta1,theta2){
	#Given two vectors of angles, compute the smallest difference between them
	diff.theta <- theta2-theta1
	(diff.theta>(-pi)&diff.theta<=pi)*diff.theta+
	(diff.theta>pi)*(diff.theta-2*pi)+
	(diff.theta<=(-pi))*(diff.theta+2*pi)
}

theta.standardized <- function(theta){
	#Standardize theta between (-pi,pi]; especially useful for the result of a call to turning.angle.function
	beta<-(theta<=pi&theta>-pi)*(theta)+(theta>pi)*(-(2*pi-theta))+(theta<=(-pi))*(2*pi+theta)
	return(beta)
}

linear.interp=function(x){
	#For linear interpoxion of missing values
	x.finite.index=which(is.finite(x))
	x.na.index=setdiff(seq(1,length(x)),x.finite.index)
	x.na.index.diff=diff(x.na.index)

	while(sum(is.na(x))>0){
		x.finite.difference=x.finite.index-x.na.index[1]
		lower.closest.index=x.na.index[1]+max(x.finite.difference[x.finite.difference<0])
		upper.closest.index=x.na.index[1]+min(x.finite.difference[x.finite.difference>0])
		block.length=length((lower.closest.index+1):(upper.closest.index-1))
		#Interpolate values
		delta.x=(x[upper.closest.index]-x[lower.closest.index])/(block.length+1)
		x[(lower.closest.index+1):(upper.closest.index-1)]=x[lower.closest.index]+delta.x*seq(1,block.length)
		#Update index vectors
		x.finite.index=which(is.finite(x))
		x.na.index=setdiff(seq(1,length(x)),x.finite.index)
		x.na.index.diff=diff(x.na.index)
	}
	return(x)
}

haversine.sep=function(lat.1,lat.2,long.1,long.2){
	#Uses haversine formula for great circle distance; input is degrees, returns distance in km
	lat.1=lat.1*pi/180
	lat.2=lat.2*pi/180
	long.1=long.1*pi/180
	long.2=long.2*pi/180
	R = 6371
	dLat = (lat.2-lat.1)
	dLon = (long.2-long.1)
	a =sin(dLat/2)^2+cos(lat.1)*cos(lat.2)*sin(dLon/2)^2
	c = 2 *atan2(sqrt(a),sqrt(1-a)) 
	return(R * c)
}

HMM.model <- function(x,J){
	if(J==2){
		mode2.break.points=quantile(x,probs=c(.5),na.rm=T)
		mode1.init.params <- fitdistr(x[which(x<=(mode2.break.points[1]))],densfun="normal")
		mode2.init.params <- fitdistr(x[which(x>(mode2.break.points[1]))],densfun="normal")
		mean.init=as.numeric(c(mode1.init.params$estimate[1],mode2.init.params$estimate[1]))
		sd.init=as.numeric(c(mode1.init.params$estimate[2],mode2.init.params$estimate[2]))
	}
	if(J==3){
		mode3.break.points=quantile(x,probs=c(.33,.66),na.rm=T)
		mode1.init.params=fitdistr(x[which(x<=(mode3.break.points[1]))],densfun="normal")
		mode2.init.params=fitdistr(x[intersect(which(x>mode3.break.points[1]),which(x<=mode3.break.points[2]))],densfun="normal")
		mode3.init.params=fitdistr(x[which(x>mode3.break.points[2])],densfun="normal")
		mean.init=as.numeric(c(mode1.init.params$estimate[1],mode2.init.params$estimate[1],mode3.init.params$estimate[1]))
		sd.init=as.numeric(c(mode1.init.params$estimate[2],mode2.init.params$estimate[2],mode3.init.params$estimate[2]))
	}
	
	init0 <- rep(1/J,J)
	P0 <- matrix(1/J,nrow=J,ncol=J)
	b0 <- list(mu=mean.init,sigma=sd.init)
	startval <- hmmspec(init=init0, trans=P0,parms.emission=b0,dens.emission=dnorm.hsmm) 
	HMM3.1 <- try(hmmfit(x,startval,mstep=mstep.norm,lock.transition=FALSE,maxit=2000))

	P0 <- diag(.9,J)
	P0[upper.tri(P0)] <- .1/(J-1)
	P0[lower.tri(P0)] <- .1/(J-1)
	startval <- hmmspec(init=init0, trans=P0,parms.emission=b0,dens.emission=dnorm.hsmm) 
	HMM3.2 <- try(hmmfit(x,startval,mstep=mstep.norm,lock.transition=FALSE,maxit=2000))

	if(class(HMM3.1)=="try-error"&class(HMM3.2)=="hmm"){HMM3=HMM3.2}
	if(class(HMM3.2)=="try-error"&class(HMM3.1)=="hmm"){HMM3=HMM3.1}
	if(class(HMM3.1)=="hmm"&class(HMM3.2)=="hmm"){
		if(HMM3.1$loglik[length(HMM3.1$loglik)]>HMM3.2$loglik[length(HMM3.2$loglik)]){HMM3=HMM3.1}else{HMM3=HMM3.2}
	}
	HMM.Vp=HMM3

	Vp.match.Viterbi <- predict.hmm(HMM.Vp,newdata=x,method="viterbi")
	Vp.match.smooth <- predict.hmm(HMM.Vp,newdata=x,method="smoothed")
	
	if(J==2){
		mode1.model.index <- which.min(HMM.Vp$model$parms.emission$mu)
		mode2.model.index <- which.max(HMM.Vp$model$parms.emission$mu)
		mode1.index <- which(Vp.match.smooth$s==mode1.model.index)
		mode2.index <- which(Vp.match.smooth$s==mode2.model.index)
		BH <- rep(NA,length(Vp.match.smooth$s))
		BH[mode1.index] <- 1; BH[mode2.index] <- 2
	}
	if(J==3){
		mode1.model.index <- which.min(HMM.Vp$model$parms.emission$mu)
		mode3.model.index <- which.max(HMM.Vp$model$parms.emission$mu)
		mode2.model.index <- setdiff(seq(1:3),c(mode1.model.index,mode3.model.index))
		mode1.index <- which(Vp.match.smooth$s==mode1.model.index)
		mode2.index <- which(Vp.match.smooth$s==mode2.model.index)
		mode3.index <- which(Vp.match.smooth$s==mode3.model.index)
		BH=rep(NA,length(Vp.match.smooth$s))
		BH[mode1.index] <- 1; BH[mode2.index] <- 2; BH[mode3.index] <- 3		
	}
	
	return(list(model=HMM.Vp,HMM.Vp=Vp.match.Viterbi,BH=BH))
}

	#MAIN
	
	#THINGS YOU NEED TO ENTER IN
	#load("/Users/arianasp/Desktop/Baboons/R_data/troop_centroid_and_bouts") #load in relevant data file
	
	tc <- Judy[,3:4] #individual position over time (x,y coords - arbitrary origin)
	idxs.to.use<-1:nrow(tc) #In case you want to use a subset of time points, enter them here (otherwise, just enter 1:length(tc)
	J<-3 # J is the number of states to fit. I think the code works for J = 2 or J = 3.
	
	
	#function to get step lengths
	sl.function <- function(x,y){
		x1 <- x[-length(x)]
		x2 <- x[-1]
		y1 <- y[-length(y)]
		y2 <- y[-1]
		z <- sqrt((x2-x1)^2+(y2-y1)^2)
		return(z)
	}
	
	#compute step length at each time point
	sl.match <- c(sl.function(x=tc[,1],y=tc[,2]),NA)
	plot(sl.match,type="l")  # First few points good?
	which(sl.match==max(sl.match,na.rm=T))
	
	
	#Compute absolute heading
	ab <- abs.direction.function(x1=tc[1:(nrow(tc)-1),1],x2=tc[2:nrow(tc),1],y1=tc[1:(nrow(tc)-1),2],y2=tc[2:nrow(tc),2])
	ab.match <- c(ab,NA)
	#plot(data.recon.sub$ab,type="l")
	
	#Turning angle
	ta.match <- ab.match[-length(ab.match)]
	ta.match <- turning.angle.function(ta.match[1:(length(ta.match)-1)],ta.match[2:length(ta.match)])
	ta.match <- c(NA,theta.standardized(ta.match),NA)
	#plot(ta.match,type="l")

	# Persistence velocity
	pv <- sl.match*cos(ta.match)
	#plot(pv,type="l")
	pv.good <- pv[idxs.to.use]
	#plot(pv.good,type="l")
	
	# HMM of persistence velocity
	pv.HMM.model <- HMM.model(pv.good,J=J)
	
	names(pv.HMM.model)
	head(pv.HMM.model$BH)
	table(pv.HMM.model$BH)
	
	#make a plot that shows the state over time (indicated by colors)
	hmm.states<-as.matrix(pv.HMM.model$BH) #get HMM states
	
	#set colors (red is the lowest pers vel state, green is highest)
	if(J==2){ 
		colors.hmm = c('red','green') 
		col1<-'red' 
		col2<-'green'
	}
	if(J==3){ colors.hmm = c('red','black','green') 
		col1<-'red' 
		col2<-'black' 
		col3<-'green'
	}
	
	#make the plot
	quartz()
	image(x=1:length(hmm.states),z=hmm.states,col=colors.hmm,xlab='time idx',ylab='HMM states')
	
	#plot distributions of step lengths and turning angles in each state 
	quartz()
	par(mfrow=c(J,2))
	hist.bins<-seq(0,10,.1)
	hist(sl.match[idxs.to.use][which(pv.HMM.model$BH==1)],xlim=c(0,2),breaks=hist.bins,col=col1)
	rose.diag(ta.match[idxs.to.use][which(pv.HMM.model$BH==1&is.finite(ta.match[idxs.to.use]))],bins=20)
	hist(sl.match[idxs.to.use][which(pv.HMM.model$BH==2)],xlim=c(0,2),breaks=hist.bins,col=col2)
	rose.diag(ta.match[idxs.to.use][which(pv.HMM.model$BH==2&is.finite(ta.match[idxs.to.use]))],bins=20)
	if(J==3){
		hist(sl.match[idxs.to.use][which(pv.HMM.model$BH==3)],xlim=c(0,2),breaks=hist.bins,col=col3)
		rose.diag(ta.match[idxs.to.use][which(pv.HMM.model$BH==3&is.finite(ta.match[idxs.to.use]))],bins=20)
	}
	




