cnh
===

Data analysis for west coast commercial fisheries

+ `rawData/` for raw data, read-only
+ `processedDate/` for processed data, separate directories for each type, catch (metier-type) and spatial (vms, spatial coastlines, MPAs, etc.). All are _technically_ disposable, although some pipes take a long time to run. 
+ `src/` code that processed `rawData` to `processedData`
