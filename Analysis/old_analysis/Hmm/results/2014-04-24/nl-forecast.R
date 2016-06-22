# nl-forecast.R
# Copyright (c) 2014 Colin Twomey <crtwomey@gmail.com>
#
# Implementation of Sugihara's simplex[1] and S-map[2] methods
# for nonlinear time-series forecasting. The code is organized
# in four sections: code common to both bethods, code for each
# method (simplex and s-map), and lastly a test case (the tent
# map example used in [1]).
#
# 1. Sugihara & May (1990) Nature, 344:734-741.
# 2. Sugihara (1994) Phil. Trans. R. Soc. Lond. A, 348:477-495.
#
# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation
# files (the "Software"), to deal in the Software without
# restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to
# whom the Software is furnished to do so, subject to the
# following conditions:
#
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
# OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
# HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
# WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
# OTHER DEALINGS IN THE SOFTWARE.


# common
# --------------------------------------------------------------------------

distance <- function(X, x) {
	sqrt(rowSums((X-matrix(x, nrow(X), ncol(X), byrow=TRUE))^2))
}

weighting <- function(D, D.mean, theta) {
	exp(-theta * (D/D.mean))
}

# notes on embedded time series (Ets) structure:
# 	Ets[1,1]  = ts[(E-1)*tau + 1]
# 	nrow(Ets) = (E-1)*tau
embed.ts <- function(ts, E, tau=1)
{
	T <- length(ts)
	t.lags <- seq(1, E*tau, tau)
	max.t.lag <- max(t.lags)

	Ets <- c()
	for (t in rev(t.lags)) {
		Ets <- cbind(Ets, ts[t:(T-max.t.lag+t)])
	}

	return(Ets)
}


# simplex method
# --------------------------------------------------------------------------

nearest.neighbors <- function(D, k) {
	which(rank(D) <= k)
}

simplex.projection <- function(nn, w.nn) {
	sum(w.nn * nn)/sum(w.nn)
}

simplex.forecast <- function(X, Y, dt, E,
	       tau = 1,
	     theta = 0.0)
{
	EX    <- embed.ts(X, E, tau)
	EY    <- embed.ts(Y, E, tau)
	D.mean <- mean(sqrt(rowSums(diff(EX)^2)))
	pred.y <- rep(0, nrow(EY))

	for (t in 1:nrow(EY)) {
		D    <- distance(EX, EY[t,])
		t.nn <- nearest.neighbors(D, E+1)
		w.nn <- weighting(D[t.nn], D.mean, theta)

		# (E-1)*tau offset needed because EX is shortened due to emebedding
		pred.y[t] <- simplex.projection(X[t.nn+dt+(E-1)*tau], w.nn)
	}

	return(pred.y)
}


# s-map method
# --------------------------------------------------------------------------

compute.C <- function(EX, EYt, dt, theta, D.mean)
{
	T <- nrow(EX)
	M <- ncol(EX)

	D <- distance(EX, EYt)
	w <- weighting(D, D.mean, theta)
	W <- matrix(w, T, M)

	A <- W[1:(T-dt),] * EX[1:(T-dt),]
	B <- w[1:(T-dt)] * EX[(1+dt):T,2]

	s <- svd(A)
	Ainv <- s$v %*% diag(s$d^-1) %*% t(s$u)
	C <- Ainv %*% B

	return(C)
}

s.map.projection <- function(EYt, C) {
	sum(C * EYt)
}

s.map.forecast <- function(X, Y, dt, E,
	       tau = 1,
	     theta = 0.0)
{
	EX    <- cbind(rep(1, length(X)-E+1), embed.ts(X, E, tau))
	EY    <- cbind(rep(1, length(Y)-E+1), embed.ts(Y, E, tau))
	D.mean <- mean(sqrt(rowSums(diff(EX)^2)))
	pred.y <- rep(0, nrow(EY))

	for (t in 1:nrow(EY)) {
		C <- compute.C(EX, EY[t,], dt, theta, D.mean)

		pred.y[t] <- s.map.projection(EY[t,], C)
	}

	return(pred.y)
}


# test case: tent map
# --------------------------------------------------------------------------

tent.map <- function(xt) {
	if (xt < 0.5) {
		return(2*xt)
	} else if (0.5 <= xt) {
		# without the 1e-8 fudging R gives us something pathological
		return(2*((1-1e-8) - xt))
	}
	return(-Inf)
}

# generate the tent-map training (X) and test (Y) time series
T <- 1000
X <- runif(1)
for (t in 1:(T+1)) {
	X <- c(X, tent.map(X[t]))
}
Xt <- diff(X)
Y  <- diff(X[(1:(T/2+1))+T/2])
X  <- diff(X[1:(T/2+1)])

# generate forecast for Y
E     <- 3
tau   <- 1
dt    <- 2
theta <- 500

Y.simplex <- simplex.forecast(X, Y, dt, E, tau, theta)
Y.smap    <- s.map.forecast(X, Y, dt, E, tau, theta)

# plot results
op <- par(mar=c(5,5,1,1), mfrow=c(1,3))

plot(Xt, type='l', las=1,
	xlab=expression(t),
	ylab=expression(Delta[t])
)

Y.predicted <- list(Y.simplex, Y.smap)
Y.label     <- list("simplex", "s-map")

for (i in 1:length(Y.predicted)) {
	Yp    <- Y.predicted[[i]]
	Ty    <- length(Y)
	Tp    <- length(Yp)
	#label <- paste(" predicted (", method.label[[i]], ")", sep='')
	label <- paste(" predicted (", Y.label[[i]], ")", sep='')

	plot(Y[((E-1)*tau+dt+1):Ty], Yp[1:(Tp-dt)], pch=16, cex=0.4, las=1,
		xlab=expression(paste(Delta[t], " observed")),
		ylab=bquote(Delta[t] ~ .(label)),
		xlim=c(-1,0.5),
		ylim=c(-1,0.5)
	)
	abline(a=0,b=1)
	abline(h=c(0,-0.5), lty=2)
	abline(v=c(0,-0.5), lty=2)	
}

par(op)
