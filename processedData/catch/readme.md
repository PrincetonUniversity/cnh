`1_cleaningData/`

+ Contains code for going from raw fish ticket data as dispersed by PacFin
+ Contains the following scripts
  + `01_incomeFilter.R`: adjust income for 2009 (removes inflation); drops vessels with less than $10,000 median income
  + `02_filter_rare.R`: combines nominal to real market categories. This script also drops dredge gear because there's only like 3 vessels. Data is now ready for clustering. This data is saved as `filtered_ftl.RDS`
  + `03_sewTrips.R`: takes the clusters that I get after running infoMap and merges that back into the ticket data. Now there's a new column called `metier` that is made up of the trip's `grgroup`.
  + `04_ARI.R`: takes the designations from both 2010 and 2012 classifications and compares agreement between the two using ARI and makes a bipartite network plot to visualize the mapping




## old stuff
No longer computing `annualRev` type analyses here. Instead doing this in analysis script that is seperate from constructing a landings dataset with realized fisheriers attached.

Also haven't re-done the clusters using revenue data yet.


The final tickets dataframe is saved as `tickets_plus.RDS` and has both metiers and strategy ids.

  + `04_annualRev.R`: calculates the yearly simpson and shannon-weiner revenue diversity, yearly revenue (only for species that were included in the clustering to begin with), and then takes the yearly average of these values. Saves as `yrdf.RDS` Because it's using `tickets_plus.RDS` to do this aggregation, it also includes strategy ID in the across year summary (if a vessel participated in only one fishery)

Added: code to link catch data to state/federal registration data to get vessel characteristics.
