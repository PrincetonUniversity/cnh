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
+ for each file it projects the trajectory into an Equal Area Lambers projection from projectionwizard.com. This is required to use the `gDistance()` function (more later).
+ It defines trips by measuring the distance between the west coast shoreline (also projected) and each VMS ping. Any distance > 1.5 km is labeled as an "at sea" point. Through some cumulative sums and differencing, this ultimately creates a column `only_trips` which has 0s for when the boat is not at sea, and odd numbers for each trip.
+ Then I link to trips, description, results and validation (to some extent) is written up in `04_link_mets_results.R`. Results in 6 new columns that have trip_ids (if any) otherwise NAs
+ whole trajectory is saved for linking with Obs data. 
