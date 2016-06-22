# crab model

library(deSolve)

LotVmod <- function(Time, State, Pars){
  with(as.list(c(State, Pars)), {
    dx = x*(alpha - beta*y)
    dy = -y*(gamma - delta*x)
    return(list(c(dx, dy)))
  })
}

Pars <- c(alpha = 2, beta = .5, gamma = .2, delta = .6)
State <- c(x = 10, y = 10)
Time <- seq(0,100, by = 1)

out <- as.data.frame(ode(func = LotVmod, y = State, parms = Pars, time = Time))

matplot(out[,-1], type = "l", xlab = "time", ylab = "population")
legend("topright", c("Cute bunnies", "Rabid foxes"), lty = c(1,2), col = c(1,2), box.lwd = 0)

#####
Crab_mod <- function(Time, State, Pars){
  with(as.list(c(State, Pars)), {
    price = 1/(b + s) + base
    feeding_b = R/(1+h_b*R)
    feeding_s = R/(1+h_s*R)
#     b <- ifelse(b < 0, 0, b)
#     s <- ifelse(s < 0, 0, s)
#     R <- ifelse(R < 0, 0, R)
#     price <- ifelse(price < 0, 0, price)
#     feeding_b <- ifelse(feeding_b < 0, 0, feeding_b)
#     feeding_s <- ifelse(feeding_s < 0, 0, feeding_s)
    db = (price*feeding_b - cost_b)*b
    ds = (price*feeding_s - cost_s)*s
    dR = R - feeding_b*b - feeding_s*s

    return(list(c(db, ds, dR)))
  })
}

Pars <- c(base = 4, h_b = .8, h_s = 1.2, cost_b = 3, cost_s = .2)
State <- c(b = 10, s = 10, R = 5000)
Time <- seq(0,20, by = 1)

out_crab <- as.data.frame(ode(func = Crab_mod, y = State, parms = Pars, time = Time))

matplot(out_crab[,-1], type='l', xlab = "time", ylab = "population", col=c("red","blue","green"), lwd=3)
