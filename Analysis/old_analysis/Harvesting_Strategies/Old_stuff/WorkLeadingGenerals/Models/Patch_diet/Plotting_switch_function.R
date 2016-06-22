# Graphing switching function from Abrams et al. 2012

diff = seq(-0.05,.05,0.001)    # difference between the two patches (Wij and Wj)
m = 0.0005        # baseline per capita rate of movement 
lambda = 250      # measures how sensitive population is to differences in patch quality
switching = m*exp(lambda*(diff))*diff

plot(diff,switching,'l',lwd=2,frame=FALSE,ylab="Probability of switching from patch i to patch j",xlab="Difference in quality between i and j")

diff = seq(0,.05,0.001)    # difference between the two patches (Wij and Wj)
m = 0.0005        # baseline per capita rate of movement 
lambda = 350      # measures how sensitive population is to differences in patch quality
switching = m*exp(lambda*(diff))*diff

lines(diff,switching,lwd=2,col="purple")

diff = seq(0,.05,0.001)    # difference between the two patches (Wij and Wj)
m = 0.005        # baseline per capita rate of movement 
lambda = 350      # measures how sensitive population is to differences in patch quality
switching = m*exp(lambda*(diff))*diff

lines(diff,switching,lwd=2,col="red")
legend('topleft',legend=c("m = 0.0505; lambda = 250","m = 0.0005; lambda = 250","m = 0.005; lambda = 350"),lwd=2,col=c("black","purple","red"),bty="n",cex=0.75)


## local knowledge switching function
alpha = 0.1
m = 0.1
lambda = 0.1
N =  -100:1000
c_S = 50
c_H = 0.5
p_S = 1
pi_i = N*(p_S - c_H)
lambda = 0.1
difference = m*exp(-lambda*(pi_i-c_S))/(1+m*alpha*exp(-lambda*(pi_i-c_S)))
plot(pi_i,difference,'l',xlab="profit in patch i",ylab="rate of moving to patch j")

                                        