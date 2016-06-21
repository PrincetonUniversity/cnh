## Abrams et al. 2013 MSY model
# playing with ODEs
require(deSolve)
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
C = .5 #0.5   # parameter that should not drive population to extinction
E = 0.25  # " "
b = .05 # (adjusted)

pars  <- c(attackrate   = C,    # /day, rate of ingestion
           r            = r,    # /day, growth rate of prey
           rMort        = d ,   # /day, mortality rate of predator
           k            = k,     # mmol/m3, carrying capacity
           catchability = q,
           effort       = E)

yini  <- c(Prey = 2, Predator = 1)
times <- seq(0, 200, by = 1)
out   <- ode(yini, times, LVmod, pars)

plot(out)