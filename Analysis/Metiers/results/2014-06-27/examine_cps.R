# look at catch profiles of mgmt_grps more than just centroids

# load data
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/cluster_sol_objectives_asw.Rdata")
load("/Volumes/NOAA_Data/CNH/Analysis/Metiers/data/all_tickets/propTable_tickets.Rdata")

source("/Volumes/NOAA_Data/CNH/Analysis/Metiers/bin/plot_clusters.R")

pdf(file="/Volumes/NOAA_Data/CNH/Analysis/Metiers/results/2014-06-27/cp_composition.pdf",width=14, height=10)
plot_cp(target_mgmt = "SAMN",cp = 7, prop_table = prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "CRAB", cp = 1, prop_table=prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "SRMP", cp = 6, prop_table=prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "SHLL", cp = 4, prop_table=prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "GRND", cp = 8, prop_table = prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "HMSP", cp = 2, prop_table=prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "OTHR", cp = 3, prop_table=prop_table, cluster_ind=cluster_ind)
plot_cp(target_mgmt = "CPEL", cp = 5, prop_table=prop_table, cluster_ind=cluster_ind)
dev.off()