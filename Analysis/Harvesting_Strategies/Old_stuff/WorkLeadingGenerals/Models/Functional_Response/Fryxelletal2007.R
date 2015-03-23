## Looking at differences between grouped pred/prey and null functional responses

# regular type II functional response

A = seq(0.1,1,.1)

a = A[1]
h1 = 0.045
h2 = 1.422
N = 0:100

psi = (a*N)/(1+a*(h1+h2)*N)

plot(N,psi,'l',lwd=2,col="blue",frame = FALSE)

color = c("blue","red","orange","yellow","green","purple","grey")

for(i in 2:length(A)){
a = A[i]
psi = (a*N)/(1+a*(h1+h2)*N)
lines(N,psi,lwd=2,col=color[i])
}

## Grouped lion functional response
# Now use an intermediate value of a but see how varying group size changes things. 

G = c(1,5,15)
a = 0.4
i =1
psi = (a*N)/(G[i]+a*(G[i]*h1+h2)*N)
plot(N,psi,type="l",lwd=2,col=i,frame=FALSE)
for(i in 2:length(G)){
psi = (a*N)/(G[i]+a*(G[i]*h1+h2)*N)
lines(N,psi,lwd=2,col=i)
}
legend("bottomright",legend=c("G = 1","G = 5","G = 15"),lwd=2,col=c(1,2,3),bty="n")

## Prey grouping. 
## varying b
b = 1
c = .1
encounter = a*c*N^b
plot(N,encounter,'l',)
b = seq(.1,1,.1)
for(i in 1:length(b)){
	encounter = a*c*N^b[i]
	lines(N,encounter,lwd=2,col = i)
}

b = .3
c = 1
encounter = a*c*N^b
plot(N,encounter,'l',lwd=2,frame=FALSE)

c = seq(.1,1,.1)	## varying c
for(i in 1:length(c)){
	encounter = a*c[i]*N^b
	lines(N,encounter,lwd=2,col=i)
}

## grouping predators and prey
G = 15
a = 0.4
b = 0.2
c = .8
psi = (a*c*N^b)/(G+a*(h1+G*h2)*c*N^b)
plot(N,psi,'l',lwd=2,frame=FALSE)


## Marc Mangel's functional response
cmax = 0.4
q = 0.5
E = 1
C0 = 1
N = 0:100

psi = (cmax*q*E*N)/(q*E*N+C0)

plot(N,psi,'l',frame=FALSE,lwd=2)

E = 0.3
psi = (cmax*q*E*N)/(q*E*N+C0)

lines(N,psi,lwd=2,col="green")