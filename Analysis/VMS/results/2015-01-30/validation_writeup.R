#' # How to validate backwards assignment method
#' 
#' The interest is, how similar is it to vessels with trips that are observed. 
#' 
#' + This metric would be $\pm$ days from departure and return on average. 
#' + Whether the listed fish-ticket matched my assigned
#' + And if there is a period of time that has been "over assigned" using my method, how far did the vessel travel during that period (distance of path).
#' + Record how long the actual trip was using observer data, in order to see the extra difference in relation to the "true" trip. 
#' 
#' Because a vessel may be observed more than once, I'll have entries for the same vessel more than once. 
#' 
#' After gathering this information, 
#' 
#' + make histogram of days $\pm$ on departures
#' + make histogram of days $\pm$ on returns
#' + make histogram of extra distance
#' + plot days additional and movement
#' 
#' I hope that there's almost no distance moved, and that the days are around 1 or 2. At a minimum, this gives an estimate of the error. I don't have a good cut-off of what's not acceptable. Obviously the concern is that there is movement being accorded to a trip that is not related.  
#' 
#' ## If outliers...
#' Investigate how this varies with gear type, vessel identity, or possibly strategy, or total number of trips if there are . 