# roadtrip

library(ggmap)
legs_df <- route('Seattle, WA, USA', 'Astoria, OR, USA',mode ="driving")
legs2 <- route( 'Astoria, OR, USA','Coos Bay, OR, USA', mode ="driving")
legs25 <- route('Coos Bay, OR, USA',"Crescent City, CA", mode ="driving")
legs3 <- route('Crescent City, CA', 'Eureka, CA, USA', mode = 'driving')
legs35 <- route('Eureka, CA', 'Fort Bragg, CA, USA', mode = 'driving')
legs4 <- route('Fort Bragg, CA, USA','San Francisco, CA, USA', mode = 'driving')
legs5 <- route('San Francisco, CA, USA', 'Half Moon Bay, CA, USA', mode = 'driving')
legs6 <- route('Half Moon Bay, CA, USA','Monteray, CA, USA', mode = 'driving')
legs7 <- route('Monteray, CA, USA','Santa Barbara, CA, USA', mode = 'driving')

legs <- rbind(legs_df, legs2,legs25, legs3, legs35, legs4, legs5, legs6, legs7)

cities <- c("Seattle, WA","Astoria, OR", "Tillamook, OR","Coos Bay, OR", "Eureka, CA", "Crescent City, CA", "Fort Bragg, CA", "San Francisco, CA", "Half Moon Bay, CA", "Monteray, CA", "Santa Barbara, CA")

city_locs <- geocode(cities)

par(oma=rep(0,4),mai=rep(0,4), bg = "transparent")
plot(legs$startLon, legs$startLat, asp = 1,type='l', col = "white")
map('state',add=T, fill = T, col="#859900", bor = "#eee8d5")
lines(legs$startLon, legs$startLat,col="#d33682",lwd=3)
points(city_locs$lon, city_locs$lat, pch = 19, col = "#d33682")
text((city_locs$lon-c(3.5,rep(2.5,10))), city_locs$lat, cities, col = "#073642")
