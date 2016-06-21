# redoing metier plots

metiers <- readRDS("metiers.RDS")
rm(list=setdiff(ls(),"metiers"))

slopes <- list()
strategies <- list()
years <- 2009:2013
par(mfrow=c(2,3))
for(i in 1:length(years)){
  cat(years[i],":\n")
  out_strategies <- find_strategies(year = years[i], df = metiers)
  cat("strategies found, ")
  strategies[[i]] <- calculate_strategy_stats(ic = out_strategies[["ic"]], 
                                              port_trips = out_strategies[["port_trips"]], 
                                              cast_trips=out_strategies[["cast_trips"]])
  cat("stats calculated, ")
  plot_strategies(strategies[[i]][["strategy"]], years[i], yr_trips = strategies[[i]][["yr_trips"]])
  slopes[[i]] <- get_slope(strategy = strategies[[i]][["strategy"]])
  cat("slope found. done.\n")
  
}

plot(years, unlist(slopes), type = "o",pch=19, bty="n", xlab="Year",ylab="slope", asp = 0)


par(mfrow=c(2,3))
for(i in 1:length(strategies)){
  plot(log(strategies[[i]][["strategy"]]$mean_rev)/log(strategies[[i]][["strategy"]]$sd_rev), strategies[[i]][["strategy"]]$mean_div, type="p",pch = 19, bty = "n", xlab = "log(mean revenue)/log(sd revenue)", ylab = "mean sw index", main = years[i])
}
