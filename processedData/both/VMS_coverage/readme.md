`VMS_coverage.R`

+ Loads ticket data
+ Loads VMS data
+ For each metier in ticket data, loops through and finds all unique `trip_id`s in landings and VMS data.
+ Counts the number of `trip_id`s that are in both
+ reports that
+ saves that as `VMS_coverage.csv`
