### Contains code to clean up raw VMS data


`01_processing_VMS.R`

+ converts the minutes/seconds of lat/lon to decimal degrees
+ removes leading/trailing whitespace from that come with fixed width files
+ VMS data also had weird headers/tails of meta-data from the database it got pulled from. Remove that
+ re-names columns into intelligible names
+ write outs RDS files that are the same size as the original fixed width files (so a file does not correspond to a single vessels info)

`02_cleaning_data.R`

+ Reads all of of the `.RDS` files, and binds them together.
+ Gives a projection to the data
+ removes data with points smaller than -180 longitude
+ overlays the west coast polygon and labels points that are "onland" (this takes a long time)
+ proceeds by vessel and finds any sequential onland points and removes the middle chunk of them (so keeps the first and last onland point).
+ splits the data by vessel track and saves them into their own files
+ merges them with `doc.num` provided by OLE.
+ removes any points with speeds > 25
+ Saves as `.RDS files` (into `02_cleaned/`)

`03_overlapMetier.R`

+ Loads data from `02_cleaned/` and looks to see if it exists in fish ticket data. And that there are at least some landings that overlap with the coverage of VMS data (max/min `date.time`)
+ Saves the files into `03_matchMetier/` and prepends a `v_` to the doc.num/drvid for the vessel for easier finding and data loading.

`04_link_mets_vms.R`

+ loops through each vessel track from `03_overlapMetier`
+ I check for observer trips which are linked with multiple fish tickets and remove those trips. This is because with two trips landings of potentially different fisheries, it's impossible to put this observed trip into a training set for one fishery or the other. So these are flagged as `obs_dup = 1`
+ I also drop any data points with longitude > 0 or < -150 (some huge outliers found manually in the VMS data, this cleans them up).
+ for each file it projects the trajectory into an Equal Area Lambers projection from projectionwizard.com. This is required to use the `gDistance()` function.
+ It defines trips by measuring the distance between the west coast shoreline (also projected) and each VMS ping. Any distance > 1.5 km is labeled as an "at sea" point. Through some cumulative sums and differencing, this ultimately creates a column `only_trips` which has 0s for when the boat is not at sea, and odd numbers for each trip.
+ Then I link to trips, I find each fish ticket for the vessel in fish tickets, and based on the "time window" argument (0, 24, 36, 72, 168), I match VMS fishing trips that land within a time window prior to the fish ticket being reported. If more than one VMS trip occurred in that time window, both are assigned to the same fish tickets. Similarly, if more than one fish ticket occurs on the same day, both fish tickets are assigned to the same VMS trips.
+ I also calculate revenue, total lbs, and several measures of revenue/lbs per unit effort. I calculate the total distance a vessel travels and the total time spent on the water. These are `lbs_time`, `rev_time`, `lbs_dist`, etc. I also add the total number of fish tickets that were combined, that's `n.trips` and revenue and lbs are also standardized by that. These values are saved for each lat/lon point on a fishing trip. This is incase a trip is collapsed to a single point.
+ whole trajectory is saved for linking with Obs data.

`05_make_obs_to_vms.R`

+ goes through each trajectory in `04_link_mets_vms\` and finds if any trips are observed. If they are, then finds the intervals of time when the boat had set gear. For all VMS points between the down and up, is labeled.
+ There are a number of fixed-gear trips that either have set times before they say they’ve departed port or up times that are after they’ve returned. These get saved in `processedData/both/obs_data/mismatch_time.csv`
+ Also when looking for intervals of time letting vms be +/- 1 hour larger than observed interval. Just because I thought it might be difficult to find the short duration fishing and that’s a reasonable error window to have as an observer.
+ Generate a file that summarizes the matched observed trips, the port, the sector, the vessel ID and the hours fished. These are saved `processedData/spatial/vms/intermediate/05_make_obs_to_vms/trip_total_tw0hr.csv` where 0 is the time window. 


`obs_seg_James/`: segmentation from James
