#########
#########

# CALCULATE ECOSYSTEM REORGANIZATION INDEX AS IN SAMHOURI ET AL. 2009 ECOSYSTEMS

# bc <- c(10,20,20,10,30)
# bs <- c(20,10,10,20,40)
# bs2 <- c(11,21,21,11,31)

eri <- function(b.control, b.scenario){ 
                # b.control: biomass of each group at steady state
                # b.scenario: biomass of each group at steady state after clamping perturbation
  
  ab_diff = abs(b.scenario - b.control);  # absolute value of biomass difference for each group
  abs_system_change = abs(sum(b.scenario) - sum(b.control)); # absolute value of difference between initial and final ecosystem biomass
  summed_abs_changes = sum(ab_diff); # absolute differences between initial and final biomass summed across all functional groups
  
  # Ecosystem reorganization index
  reorg = ((summed_abs_changes - abs_system_change)/sum(b.control))*100
  return(reorg)
}

# b.control<-em_ccc[1,]
# b.scenario<-em_zzz[1,]


# #Average relative change in entire ecosystem
# rel_change = ((b.scenario - b.control)/b.control)*100; # % change in biomass relative to initial biomass for each functional group
# avg_rel_change = rowMeans(rel_change)[[1]]; # average relative change in biomass for all groups in ecosystem
# 
# #the 2 lines below are not used for output data
# change_rel_system = ab_diff/sum(b.control); # change in each functional group relative to initial ecosystem biomass
# gChange_rel_syst_change = (ab_diff/abs_system_change) * 100; # change in group i relative to change in entire system