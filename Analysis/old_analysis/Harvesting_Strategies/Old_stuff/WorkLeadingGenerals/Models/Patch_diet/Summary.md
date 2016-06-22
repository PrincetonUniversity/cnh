


Patch-Diet Choice Model
========================================================

This whole thing comes from reading Giraldeau & Caraco (2000) and Heller (1980). Diet choice has been considered extensively in the optimal foraging literature. Specifically, the model assumes that an individual encounters different prey items at a constant rate, proportional to the density of that type. These prey types are then ranked based on profitability (ratio of net energy per item to handling time per item), and the model seeks to find the combination that maximizes long term energy gain. 

However if other foraging individuals are present, it's likely that they might affect the probability of encountering a prey item or change the handling time. The most likely effects are reducing local density of prey and increasing a forager's uncertainty about the local relative abundances of different prey types. So the question is, what happens when prey types are being depeleted in a nonuniform manner? 

On optimal diet in a patchy environment: Heller, 1980
-----------------------------------------------------------
Heller (1980) simulates diet choice when a solitary individual can forage in two depletable patches that each have two different prey choices. Using a few different strategies he examines when maximizing the instaneous gain rate and the long-term intake differ. 


Current Simple Model --- extension of Abrams _et. al._ (2012)
------------------------------------------------------------------------------------
Abrams _et. al._ (2012) uses a 2 patch predator-prey metapopulation model where harvesting only occurs on predators in one patch, prey do not move. The model is as follows:
Prey ($R$) grows logistically in patch $i$ and is eaten by predators ($N_i$) according to either linear or Holling Type II function ($g[C_i,R_i]$). 
\[\frac{dR_i}{dt}=R_i(r_i-k_iR_i)-N_ig[C_i,R_i]\]
Predators in patch $i$ ($N_i$) grow depending on how much they eat (modified by some uptake parameter $b$), their death rate, the harvesting rate in patch $i$, and either come from or leave for the other patch $j$ depending on the differences in fitness between the two patches ($W_i$ and $W_j$).
\[\frac{dN_i}{dt}=N_i(bg[C_i,R_i]-d_i-H_i)-N_iM_{ij}[W_j-W_i]+N_jM_{ji}[W_i-W_j]\]
Predation term can either be linear or Type II Holling function as described above with $C$ as the attack rate in patch $i$,
\[g[C_i,R_i]=\frac{C_iR_i}{1+C_ihR_i}\text{ or }C_iR_i\]
Fitness of patch $i$ is determined by how much a predator will eat, the death rate rate, and the harvesting rate in patch $i$. $\phi_i$ is a binary parameter (either 0, 1) that determines if the harvesting is perceived by the predator. If $\phi_i=0$, the predator the harvesting is invisible to the predator. 
\[W_i = bg[C_i,R_i]-di-H_i\phi_i\]
$M_{ij}$ is the sensitivity of the predator to differencs between the patches. $m$ is the baseline movement rate, and $\lambda$ is the fitness sensitivity. 
\[M_{ij} = me^{\lambda(W_j-W_i)}\]
Abrams _et. al._ (2012) find that if predators move adaptively (i.e. follow fitness gradients) quickly and harvesting is invisible, it's possible to get global extinction from only harvesting one patch. Further these extinction events can happen quite suddenly, and multiple stable states are common for a wide-rang of parameters. The authors suggest this might have occurred with the cod collapses in the Northeast Atlantic. 

### My extension
What happens if I let $H_i$ vary with conditions in patch $i$. In other words, what if you can harvest at both patches, and harvesters are also allowed to follow local gradients. What changes? To do this, I add one more equation for the fraction of harvesters in patch $i$ ($H_i$)
\[\frac{dH_i}{dt}=-H_iQ_{ij}(\pi_j-c_t-\pi_i)+H_jQ_{ji}(\pi_i-c_t-\pi_i)\]
Here $c_t$ is the cost of travel between the two patches, and $\pi_i$ is the profit gained from harvesting in patch $i$, calculated as:
\[\pi_i=(P_N-C_{S,i})N_i\]
Where $C_{S,i}$ is the cost of searching for predators in patch $i$ and $P_N$ is the price for predators. $Q_{ij}$ is the sensitivity to the differences between the patches and is caluclated (much like the predators) as:
\[Q_{ij}=qe^{\lambda(\pi_j-c_t-\pi_I)}\]
Where $q$ is the baseline movement rate between patches, probably will $=1$. 

I'd also like to compare the MSY version of this model to see how including patches would change things. 

#### MSY Version and Analysis
The MSY version with constant effort harvesting (assuming linear predation term):
\[\frac{dR}{dt}=R(r-kR)-NCR\]
\[\frac{dN}{dt}=bNCR-d-QEN\]
To find $N_{MSY}$ first find equilibrium:
\[\frac{dR}{dt}=0\rightarrow R^*=0\]
\[r -kR-NC=0\]
\[N^* = \frac{r-KR}{C}\]
And from $\frac{dN}{dt}$
\[\frac{dN}{dt}=0\rightarrow N^*=0\]
\[bCR-d-qE=0\]
\[R^*=\frac{d+qE}{bC}\]

Can plug in predator nullcline so nontrivial equlibrium will be:
\[R^*=\frac{d+qE}{bC},N^*=\frac{r-KR}{C}\rightarrow\frac{r-K\left(\frac{d+qE}{bC}\right)}{C}\]

Using parameters from Abrams _et. al._ (2012), figure 1, where possible

<div><img src="figure/unnamed-chunk-1.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1"  /></div>


To find $Y_{MSY}$, know that 
\[Y=qEN\]
\[Y=qE\frac{r-K\left(\frac{d+qE}{bC}\right)}{C}\]
To find maximum yield, need this to be a parabola. Graph to check:

<div><img src="figure/unnamed-chunk-2.png" title="plot of chunk unnamed-chunk-2" alt="plot of chunk unnamed-chunk-2"  /></div>

To find maximum take derivative of $Y$ wrt to $E$ and set to 0. 
\[Y = qE\frac{rbC-kd-kqE}{bC^2}\rightarrow \frac{qErbC-qEkd-kq^2E^2}{bC^2}\]
\[\frac{dY}{dE}=\frac{qr}{C}-\frac{qkd}{bC^2}-\frac{2kq^2E}{bC^2}=0\]
\[0 = \frac{r}{C}-\frac{kd}{bC^2}-\frac{2kqE}{bC^2}\]
\[\frac{2kqE}{bC^2} = \frac{r}{C}-\frac{kd}{bC^2}\]
\[2kqE=rbC-kd\]
\[E_{MSY}=\frac{rbC}{2kq}-\frac{d}{2q}\]
So $Y_{MSY}$ 
\[Y_{MSY}=q\left(\frac{rbC}{2kq}-\frac{d}{2q}\right)\left(\frac{r-K\left(\frac{d+qE}{bC}\right)}{C}\right)\]
\[Y_{MSY}=\left(\frac{rbC-dk}{2k}\right)\left(\frac{rbC-kd-kqE}{bC^2}\right)\]
Lots of algebra...
\[Y_{MSY}=\frac{r}{4}\left(\frac{rb}{k}-\frac{d}{C}\right)\]

#### Coding up and running MSY model 
Coding it up and running the model 

```r
LVmod <- function(Time, State, Pars) {
  with(as.list(c(State, Pars)), {
    Ingestion    <- attackrate  * Prey * Predator
    GrowthPrey   <- Prey * (r - k*Prey)
    MortPredator <- rMort 
    Harvest      <- catchability * effort * Predator
    
    dPrey        <- GrowthPrey - Ingestion
    dPredator    <- Ingestion*b  - MortPredator - Harvest
    
    return(list(c(dPrey, dPredator)))
  })
}


r=5 # birth rate (adjusted)
k=.7 # carrying capacity (adjusted)
d=0.02 # death rate
q=.1    # catchability, not in Abrams et al. 2013
C = .5  # parameter that should not drive population to extinction
E = 0.25  # " "
b = .05 # (adjusted)

pars  <- c(attackrate   = C,    # units?
           r            = r,    # units?
           rMort        = d ,   # units?
           k            = k,    # units
           catchability = q,
           effort       = E)

yini  <- c(Prey = 2, Predator = 1)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, LVmod, pars)

plot(out)
```

<img src="figure/unnamed-chunk-3.png" title="plot of chunk unnamed-chunk-3" alt="plot of chunk unnamed-chunk-3" style="display:block;" />

* I had to adjust the parameters quite a bit from Abrams _et. al._ (2012) in order to get the predators to stay alive.
* Also noticed that the mortality rate in the model is flat, not proportional to current number of predors present ($d$ not $dN$). 
* Not sure what the next step is. I think it's analyzing the patch version of the model. But don't know how to start with 3 differential equations. Or if the model is a good one even to invest the time to analyze... 
