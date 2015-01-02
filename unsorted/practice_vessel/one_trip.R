# plotting one trip
one_trip <- boat.dat[40:54,]

plot(one_trip$X, one_trip$Y, pch=19, type="o",asp=1, cex=0.25,col="red")
require(mapdata)
require(maps)
map("worldHires",add=T, fill=TRUE,col=c("grey","darkgrey"))

# trying to parse trips
out_port <- which(is.na(one_boat[,14]))
time_out <- diff(out_port[1:100])
plot(time_out,type="l",pch=19)

boat_sp <- move(x=boat.dat$X, y=boat.dat$y, time=as.POSIXct(boat.dat$Time, format="%Y-%m-%d %H:%M:%S", tz="UTC"), data=boat.dat, proj=CRS("+proj=longlat"), animal="Eric")
