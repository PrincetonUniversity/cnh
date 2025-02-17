# Guide to data
This directory houses spatial data (polygons, shapefiles) that is for the west coast.

`GSHHS_shp/` is a directory with shape files of the United States from [GSHHS](http://www.ngdc.noaa.gov/mgg/shorelines/gshhs.html) (A global self-consistent, hierarchical, high-resolution geography database). These shapefiles come in different resolutions and have different coverages (just mapping coastlines, to all bodies of water). We're only interested in the coastline, and that corresponds to L1 (boundary between land and ocean). The resolution of the file is i (intermediate) due to the fact that the full (f) resolution file was to giant to load into `R`.

`spatialManagement/` is a directory that houses shapefiles of areas that are either entirely or partically restricted to fishing. See `spatialManagement/README.md` for more details

`coastline.Rdata` is a spatial polygon to be loaded into R by using `load("coastline.Rdata")` which will load an object called `WC`. Try `plot(WC)` to see the polygon. This is the coast that is used to filter for port/on land for VMS tracks.

`pcid.csv` comes from [PacFin](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/pc.txt) that I just pulled off the internet nad removed the header

`wc_fishing_communities.csv` is a csv file that comes from a `.kmz` file from PacFin [here](http://www.pcouncil.org/habitat-and-communities/area-closures/) on fishing communities. This just as the location and name data stripped so I can use it more easily.
