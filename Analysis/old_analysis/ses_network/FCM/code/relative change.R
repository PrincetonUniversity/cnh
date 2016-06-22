#########
#########

# CALCULATE RELATIVE CHANGE IN EACH SCENARIO

change <- function(b.control, b.scenario){ 
                # b.control: biomass of each group at steady state
                # b.scenario: biomass of each group at steady state after clamping perturbation
  
  rel_change = ((b.scenario - b.control)/b.control)*100 # % change in biomass relative to initial biomass for each functional group
  avg_rel_change = mean(rel_change) # average relative change in biomass for all groups in ecosystem
  return(avg_rel_change)
}

# b.control<-em_ccc[1,]
# b.scenario<-em_zzz[1,]


