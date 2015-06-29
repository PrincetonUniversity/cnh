# assign OA/LE to dahl sector trips

tickets <- readRDS("/Users/efuller/1/CNH/processedData/catch/dahl_sector/tickets_dahl_sector.RDS")

oa = c("s6", "s8", "s10","s11", "s12")
le = c("s1", "s2", "s3", "s4", "s5", "s7", "s9","s20")
unknown = c("s13","s14", "s15")

tickets$access <- ifelse(tickets$dahl_sector %in% oa, "OA",
                  ifelse(tickets$dahl_sector %in% le, "LE",
                  ifelse(tickets$dahl_sector %in% unknown, "unknown", NA)))

unassigned <- subset(tickets, access %in% c("unknown",NA))

# sector 15 should be big fisheries
s15_trips <- unique(subset(unassigned, dahl_sector == "s15", select = c("trip_id","drvid","metier","cluster","dahl_sector","agid")))

metier_management <- as.matrix(table(s15_trips$metier, s15_trips$agid))
metier_management <- metier_management[order(rowSums(metier_management), decreasing = T),]
write.csv(metier_management, "/Users/efuller/1/CNH/processedData/catch/dahl_sector/s15_metiers.csv")

# HKL_1 that is in s15, probably halibut?
mystery_trips <- s15_trips$trip_id[which(s15_trips$metier=="HKL_1")]
mystery_catch <- subset(tickets, trip_id %in% mystery_trips, select = c("trip_id","modified","round_wt","adj_revenue"))
melt_df <- melt(mystery_catch, id.vars =c("trip_id", "modified"), measure.vars = "round_wt")
cast_df <- dcast(melt_df, trip_id ~ modified, fun.aggregate = sum)

# TLS_3: what are they?
alb <- s15_trips$trip_id[which(s15_trips$metier=="TLS_3")]
alb_catch <- subset(tickets, trip_id %in% alb, select = c("trip_id","modified","round_wt","grid"))
melt_alb <- melt(alb_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_alb <- dcast(melt_alb, trip_id~modified, fun.aggregate = sum)
# salmon and albacore together

# HKL_2: what's not in gf?
hkl2 <- s15_trips$trip_id[which(s15_trips$metier=="HKL_2")]
hkl2_catch <- subset(tickets, trip_id %in% hkl2, select = c("trip_id","modified","round_wt","grid"))
melt_hkl2 <- melt(hkl2_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_hkl2 <- dcast(melt_hkl2, trip_id~modified, fun.aggregate = sum)
# sport fish with their bait?

# HKL_8: what's not in gf?
hkl8 <- s15_trips$trip_id[which(s15_trips$metier=="HKL_8")]
hkl8_catch <- subset(tickets, trip_id %in% hkl8, select = c("trip_id","modified","round_wt","grid"))
melt_hkl8 <- melt(hkl8_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_hkl8 <- dcast(melt_hkl8, trip_id~modified, fun.aggregate = sum)


# TWL_1: what's not in gf
twl1 <- s15_trips$trip_id[which(s15_trips$metier=="TWL_1")]
twl1_catch <- subset(tickets, trip_id %in% twl1, select = c("trip_id","modified","round_wt","grid"))
melt_twl1 <- melt(twl1_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_twl1 <- dcast(melt_twl1, trip_id~modified, fun.aggregate = sum)

# HKL_3: what's not in gf
hkl3 <- s15_trips$trip_id[which(s15_trips$metier=="HKL_3")]
hkl3_catch <- subset(tickets, trip_id %in% hkl3, select = c("trip_id","modified","round_wt","grid"))
melt_hkl3 <- melt(hkl3_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_hkl3 <- dcast(melt_hkl3, trip_id~modified, fun.aggregate = sum)

# TWL_17: what's not in gf
twl17 <- s15_trips$trip_id[which(s15_trips$metier=="TWL_17")]
twl17_catch <- subset(tickets, trip_id %in% twl17, select = c("trip_id","modified","round_wt","grid"))
melt_twl17 <- melt(twl17_catch, id.vars = c("trip_id","modified"), measure.vars = "round_wt")
cast_twl17 <- dcast(melt_twl17, trip_id~modified, fun.aggregate = sum)
