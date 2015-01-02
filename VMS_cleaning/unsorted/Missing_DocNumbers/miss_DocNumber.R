load("/Users/efuller/Documents/Harvesting_Strategies/PaperSeeds/EmpiricalAnalysis/NOAA/Data/VMS_Related/VMS/Code/For_posterity/VMS.Rda")

library(plyr)

list of ships without a doc number
missing<-VMS[which(is.na(VMS$Doc_Number)),]
names <- unique(missing$Vessel_Name)
Ship_ID <- unique(missing$Ship_Number)

# there are more IDs than names. To find out which name has more than one ID attached make a list
dn <- vector(mode = "list", length = length(Ship_ID))
names(dn) <- Ship_ID

for(i in 1:length(Ship_ID)){
	dn[i] <- as.character(unique(missing$Vessel_Name[which(missing$Ship_Number==Ship_ID[i])]))
}

which(lapply(dn,length)==0)

whichISIT <- rep(NA, length(names))

for(i in 1:length(names)){
	whichISIT[i] <- length(which(dn==names[i]))
}

# make a list of names and Ship numbers associated with them. Four ships have two ship numbers, what's that about. 

# these ships are Michelle Renee, Progress, Alaska Trojan, and Kiska Sea 

together <- vector(mode = "list", length = length(names))
names(together) <- names

for (i in 1:length(names)){
	together[[i]] <- as.character(unique(missing$Ship_Number[which(missing$Vessel_Name==names[i])]))
}

foo <- do.call(rbind.data.frame,together)
foo_again <- data.frame(Boat = as.character(row.names(foo)), Ship_ID1 = as.character(foo[,1]), Ship_ID2 = as.character(foo[,2]))

foo_again$Boat <- as.character(foo_again$Boat)
foo_again$Ship_ID1 <- as.character(foo_again$Ship_ID1)
foo_again$Ship_ID2 <- as.character(foo_again$Ship_ID2)
find_2 <- foo_again$Ship_ID1 == foo_again$Ship_ID2

found_2 <- which(find_2==FALSE)
foo_again$Ship_ID2[-found_2]=" "

write.csv(foo_again,"Missing_DocNumbers.csv")

# make a list for all vessel names, Ship IDs and Doc numbers

all.names <- unique(VMS$Vessel_Name)

# can make nested lists? First level is name of boat. In that have list of vessel ID numbers and Doc numbers, Declarations. 
metadata <- vector(mode = "list", length = length(all.names))
names(metadata) <- all.names

for(i in 598:length(all.names)){
		Ship_IDs <- as.character(unique(VMS$Ship_Number[which(VMS$Vessel_Name==all.names[i])]))
		Doc_Numbers <- as.character(unique(VMS$Doc_Number[which(VMS$Vessel_Name==all.names[i])]))
		Declarations <- as.character(unique(VMS$Declarations[which(VMS$Vessel_Name==all.names[i])]))
		metadata[[i]] <- list(Ship_IDs = Ship_IDs, Doc_Numbers = Doc_Numbers, Declarations = Declarations)
		print(round(i/1158, digits=2))
	}

#haven't figured out a good way to summarize this list but saved nonetheless
saveRDS(metadata,file="Metadata_Vessels.rds")