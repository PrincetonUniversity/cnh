## Plotting baythmetry (taken from http://wiki.cbr.washington.edu/qerm/index.php/Bathymetry_data)

#grid data in ASCII raster format with no headers from  http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html
# note: path changed, file Bath_map2.r likely needs to have the same correction as in the two lines below
d = read.table('http://wiki.cbr.washington.edu/qerm/sites/qerm/images/a/ad/Eli_bath.txt',header=T)
d.e = read.table("/Users/efuller/Documents/Learning/Courses/AniMove2014/Course_material/Lectures/2nd_wkProject/bathymetry plotting/geodas_west_coa-5271/west_coa-5271/west_coa-5271.asc")
lonvec = seq(130,180,1/15) #4-minute grid
latvec = seq(30,70,1/15)
#d2 = d[nrow(d):1,] #why it needs to be flipped over, who knows?
d.e2=d[nrow(d.e):1,]

# new and fancier, extending to full range of topo.colors separately for land and water
#dmin=min(d2)
d.emin=min(d.e2)
#dmax=max(d2)
d.emax=min(d.e2)
#NwaterColors = round(abs(dmin))
NwaterColors = round(abs(d.emin))
#NlandColors = round(abs(dmax))
NlandColors = round(abs(d.emax))

#taking first 1/3 of topo.colors of length NwaterColors for water
waterColorVec = topo.colors(NwaterColors*3)[1:NwaterColors]
#taking second 2/3 of topo.colors of length NlandColors for land
landColorVec = topo.colors(NlandColors*3/2)[round(NlandColors/2+1):round(1.5*NlandColors)]
#combining
allColorVec = c(waterColorVec,landColorVec)

#plot in R
library(maps)
library(mapdata)
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i')
image(lonvec,latvec,t(as.matrix(d.e2)), col=allColorVec, add=T)
map('worldHires',xlim=c(140,173),ylim=c(43,63), xaxs='i',yaxs='i', add=T)
box()
axis(1, seq(140,180,5),paste(seq(140,180,5),'°E', sep=''))
axis(2, seq(30,70,5),paste(seq(30,70,5),'°N', sep=''))
