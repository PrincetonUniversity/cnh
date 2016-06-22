
# Catching up on behavioral segmentation

Angela has worked on figuring out the right classification algorithm for shrimp data which is ground-truthed. Steps she has include

From `VMS_ETL.R`

+ split by trips (using when they're no longer "onland")
+ get useful speeds (filtering out speeds that are greater than 50 knots)
+ subsets to daylight hours
+ finds breakpoints of speed
+ compute cubic hermite splien interpolation for each trip

From `VMS_ETL_stats.R`

+ helper functions to calculate speeds, angles and distances between points, also sinuosity

From `VMS_ETL_interpolate.R`

+ function definitions to interpolate VMS tracks

From `VMS_ETL_helper_fns.R`

+ functions for cleaning VMS data

From `VMS_ETL_groundtruthed.R`

+ applies cleaning functions
+ tries different approaches to classification (`knn`, `bcpa`, `em`, `hmms`)
