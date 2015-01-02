# Guide to Data

`VMS.txt` is a comma delimited file that is generated from the raw `VMS*.log` files in `/CNH/Data/VMS` by `/CNH/src/format_data.sh`. This file still needs processing: 

+ lat/lon needs to go from min/sec to decimal degrees.
+ time needs to be split up to be readable by `R`
+ flagging duplicates
+ flagging points that fall on-land
+ flagging points that are within 2 nautical miles of a port
+ remove random whitespace
+ join declarations and a second vessel ID to VMS vessels
