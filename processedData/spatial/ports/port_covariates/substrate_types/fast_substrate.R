library(sp, lib.loc = "/tigress/efuller/R_packages"); library(raster, lib.loc = "/tigress/efuller/R_packages"); library(rgdal, lib.loc = "/tigress/efuller/R_packages")
setwd("/tigress/efuller/substrate_types")

  rasterOptions(maxmemory=5e+10,chunksize=5e+9)

#maybe only need points
all_ports <- read.csv("/tigress/efuller/substrate_types/all_ports.csv",stringsAsFactors = FALSE)
hab <- raster("coastal_habitat_type1.tif")

port_locs <- all_ports[-which(is.na(all_ports$lon)),]
# remove row names
coordinates(port_locs) <- ~ lon+lat
proj4string(port_locs) <- CRS("+init=epsg:4269")
port_locs <- spTransform(port_locs, CRS(proj4string(hab)))

substrate <- extract(hab, port_locs, buffer = 100*1000, method = "simple")

saveRDS(substrate, "/tigress/efuller/substrate_types/substrate_results/substrate.RDS")

tabFunc<-function(indx, extracted) {
  dat<-as.data.frame(table(extracted[[indx]]))
  dat$pcid<-port_locs@data$Pcid[indx]
  return(dat)
}

tabs<-lapply(seq(substrate), tabFunc, substrate)
tabs<-do.call("rbind",tabs )

saveRDS(tabs, "/tigress/efuller/substrate_types/substrate_results/tabs.RDS")

library(dplyr, lib.loc = "/tigress/efuller/R_packages"); library(tidyr,lib.loc="/tigress/efuller/R_packages")
percent_substrate <- tabs%>%
  group_by(pcid) %>% # group by region
  mutate(totcells=sum(Freq), # how many cells overall
         percent.area=round(100*Freq/totcells,2)) %>% #cells by landuse/total cells
  dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
  spread(key=Var1, value=percent.area, fill=NA)

saveRDS(percent_substrate, "/tigress/efuller/substrate_types/substrate_results/percent_substrate.RDS")

substrate_measures <- cbind(spread(tabs, key = Var1, value = Freq, fill = 0),
                            percent_substrate[,2:ncol(percent_substrate)])

write.csv(substrate_measures, "/tigress/efuller/substrate_types/substrate_all.csv")
