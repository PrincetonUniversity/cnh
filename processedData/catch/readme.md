`1_cleaningData/`

+ Contains code for going from raw fish ticket data as dispersed by PacFin
+ Contains the following scripts
  + `01_incomeFilter.R`: adjust income for 2009 (removes inflation); drops vessels with less than $10,000 median income
  + `02_filter_rare.R`: drops species rarely caught and combines nominal to real market categories. This script also drops dredge gear because there's only like 3 vessels. Classify rare species as caught in fewer than 100 trips (across all five years) and the median catch of those trips is less than 100 pounds. Data is now ready for clustering. This data is saved as `filtered_ftl.RDS`
  + `03_sewTrips.R`: takes the clusters that I get after running infoMap and merges that back into the ticket data. Now there's a new column called `metier` that is made up of the trip's `grgroup` and cluster ID. [This is still a little messed up. It actually takes the output from a second round of clustering for profiles that happens in `fisheries_participation_profiles/`, need to straighten thsis out]. The point is, that the final tickets dataframe is saved as `tickets_plus.RDS` and has both metiers and strategy ids.
  + `04_annualRev.R`: calculates the yearly simpson and shannon-weiner revenue diversity, yearly revenue (only for species that were included in the clustering to begin with), and then takes the yearly average of these values. Saves as `yrdf.RDS` Because it's using `tickets_plus.RDS` to do this aggregation, it also includes strategy ID in the across year summary (if a vessel participated in only one fishery)

  Needs: to go through this and re-adjust paths and save final data frame in somewhere obvious/special.
