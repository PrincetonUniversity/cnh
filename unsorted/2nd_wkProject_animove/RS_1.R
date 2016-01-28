# date: 2014-04-11
# attempt at dynamic brownian bridge estimation for a single vessel - Draft
# also has some random stuff

load("data/all_vessels.Rdata")

# then run for loop for calculating brownian bridges for each

# should make each individual be a bursted move object, calculate a brownian bridge for each burst (within individuals), then add up all layers for individuals, normalize, and then add up all brownian bridges across all vessels. 

path= "data/"


vessel_trajs <- list()
for(i in 1:length(all_vessels)){
  file_load <- grep(paste(all_vessels[i],"_dBMMvar_",Sys.Date(),sep=""),list.files("data/"))
  load(paste(path,list.files("data/")[file_load],sep=""))
  vessel_trajs[[i]] <- brownian.bridge.dyn(dBBMM, location.error=rep(100,n.locs(dBBMM)),margin=11,window.size=31,ext=.5)
}
save(vessel_trajs,file=paste("data/vessel_trajs",Sys.Date(),".Rdata",sep=""))

## for two vessels i have, do a joint brownian bridge to make sure I know how

vars <- rep("vessel",2)

for(i in 1:2){
  var_names[i] <- paste(vars[i],i,sep='_')
}

for(i in 1:2){
  file_load <- grep(paste(all_vessels[i],"_move_",Sys.Date(),sep=""),list.files("data/"))
  load(paste(path,list.files("data/")[file_load],sep=""))
  assign(var_names[i],VMS1_move)
  
}

# burst by port for vessel 1
vessel1_b <-burst(vessel_1,head(vessel_1$behavior,-1))
vessel1_sp <- spTransform(vessel1_b,CRSobj="+proj=aeqd",center=TRUE)
vessel2_b <- burst(vessel_2,head(vessel_2$behavior,-1))

bridge1 <- brownian.bridge.dyn(vessel1_sp[vessel1_b@burstId=="port"], location.error=100,margin=11,window.size=31,ext=0.3)

vessel_list <- list(vessel1_b,vessel1_b)

#### takes a long time to run: setting this aside for the moment. 

# looking at differences in sigma


