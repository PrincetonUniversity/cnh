# Look to see if vessels present

load("1/CNH/Analysis/VMS/results/2014-10-29/3_VMSdf.Rdata")



names = c("the gayle", "westwind","papson","savannah", "tidepoint")
vessel_ids = c("30341","06202","49477","70772","32965")
vessel_no = c("44708","44690","92418","39633","92228")
operators = c("stan bruno","r farquhar", 'tuk yi',"j carvalho","scott rouhier")
vessels <- data.frame(name=names, vessel_ids = vessel_ids, vessel_no = vessel_no, operators = operators )

VMS_number <- unique(VMSdf$Ship_Number)
VMS_vesselID <- unique(VMSdf$Doc_Number)
VMS_name <- unique(tolower(VMSdf$Vessel_Name))

any(VMS_number %in% vessel_ids)
any(VMS_number %in% vessel_no)

any(VMS_vesselID %in% vessel_ids)
any(VMS_vesselID %in% vessel_no)

grep("gayle",VMS_name)
grep("westwind",VMS_name) # have this vessel
VMSdf[1060409,]

grep("papason",VMS_name)
grep("savanna",VMS_name)
grep("tidepoint",VMS_name)


new_nos <- c("71339","07376","30032","36122","07149","06738","35950","70331","07394")

any(VMS_vesselID %in% new_nos)
any(VMS_number %in% new_nos)

# hm check fish tickets
tickets <- readRDS("1/CNH/Analysis/Metiers/results/2015-01-09/code/3_exploreBuildwebs/tickets.RDS")
drvids <- unique(tickets$drvid)

any(drvids %in% new_nos)
any(drvids %in% vessel_ids)
any(drvids %in% vessel_no)

veids <- unique(tickets$veid)
unique(subset(tickets, veid %in% veids[which(veids %in% new_nos)])$metier)
veids[which(veids %in% new_nos)]
