# looking at group size distributions in haul. maybe percentage of haul and absolute numbers
library(data.table)
obs <- fread("/Users/efuller/1/CNH/Data/Observers/WCGOPobs/Samhouri_OBFTfinal_Allfisheries_ProcessedwFunction_2009-2012_110613.csv")

# look at GF IFQ trawl

# sablefish
library(RColorBrewer)
paint <- brewer.pal(n = 8, "Spectral")
with(subset(obs, sector=="Catch Shares" & spc.name=="Rex Sole"),plot(density(LB),col=paint[1],lwd=2,xlim=c(0,1000)))
with(subset(obs, sector=="Catch Shares" & spc.name=="Longnose Skate"),lines(density(LB),col=paint[2],lwd=2))
with(subset(obs, sector=="Catch Shares" & spc.name=="Sablefish"),lines(density(LB),lwd=2,col=paint[3]))
with(subset(obs, sector=="Catch Shares" & spc.name=="Dover Sole"),lines(density(LB),col=paint[4],lwd=2))
with(subset(obs, sector=="Catch Shares" & spc.name=="Shortspine Thornyhead"),lines(density(LB),col=paint[5],lwd=2))
with(subset(obs, sector=="Catch Shares" & spc.name=="Arrowtooth Flounder"),lines(density(LB),col=paint[6],lwd=2))
with(subset(obs, sector=="Catch Shares" & spc.name=="Aurora Rockfish"),lines(density(LB),col=paint[7],lwd=2))
with(subset(obs, sector=="Catch Shares" & spc.name=="Longspine Thornyhead"),lines(density(LB),col=paint[8],lwd=2))



head(sort(table(subset(obs,sector=="Catch Shares")$spc.name),decreasing=T),10)
#----
# trying to find percentage of haul -- they already calculate it!

paint <- brewer.pal(n = 8, "Set2")

fish_names <- c("Rex Sole","Longnose Skate","Sablefish","Dover Sole","Shortspine Thornyhead","Arrowtooth Flounder","Aurora Rockfish","Longspine Thornyhead")

with(subset(obs, sector=="Catch Shares" & spc.name=="Rex Sole"),plot(density(proportion,na.rm=T),col=paint[1],lwd=3))

for(i in 2:length(fish_names)){
  with(subset(obs, sector=="Catch Shares" & spc.name==fish_names[i]),lines(density(proportion, na.rm=T),col=paint[i],lwd=3))
}

legend("topright",legend=fish_names, col=paint, lwd=3, bty="n")

#----
# but actually want to know the number of species being caught together (species richness), and eveness

# look at distributions of hauls species richness

species_richness <- ddply(sub_obs, .(TRIPID, HAUL_ID), summarize, 
                          richness = length(unique(spc.name)), 
                                            haul_tot = unique(haul_tot)[1])
hist(species_richness$richness,breaks=30,col="grey",bor="slategrey")
with(species_richness, boxplot(haul_tot~richness),pch=19,cex=.5)
haul_totals <- unique(sub_obs[,c("HAUL_ID","TRIPID","haul_tot"),with=FALSE])
with(subset(haul_totals, haul_tot < 30000),hist(haul_tot,breaks=30))
