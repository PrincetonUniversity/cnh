# Order of operations

1. Generate a `filtered_ftl.RDS` locally. Adjusts income to be all in 2009 dollars. No longer removes <$10,000 annual income and rare species. Couldn't think of a good reason to drop prior to metier analysis. Can drop for actual analysis. Revenue = landed_wt * ppp. Does drop dredge trips and removes "nominal" distinction from species names. It also drops vessels that report using more than one type of gear. Drops 1689 trips. Possible that I could go back and use nearest neighbor on metiers to re-assign these trips.

(found that some spid 'modified' ids were not correct (i.e. vermillion getting split up and california halibut)) and that a bunch of trips were getting dropped because i was searching for double gear using grid in the della version of the script. that's fixed.)

(also found that there are some duplicate rows in the raw `ftl` data. Removed those, and re-ran. So now fixed. Only 33 so probably didn't matter too much)

2. Move `filtered_ftl.RDS` to tigress.  Run 1_makeLinkList.R on della to generate network input to infoMap. For particular year, gear subset, calculates pairwise hellinger distance between trips based on `adj_revenue`, makes a network with igraph, returns a link list (source, target, weight). This gets fed into infoMap. Bash scripts are all [GEAR][YEAR].sh in the `bash_scripts/` directory.

3. Then use infoMap to generate clusters. Run bash_scripts that start with `im_`. This returns .clu files.

3.5 I need to run `2_makeClusterKey.R` after the infoMap has returned clusters. I don’t currently have bash scripts to submit to della, but run just straight from command line in tigress: `Rscript 2_makeClusterKey.R “POT” 2012` for example.

4. Then use clusters returned to classify rest of trips using k-nearest neighbors. These are the bash scrips that begin with `class_`. These return `.RDS` files. **Need to add stipulation that if after classification a fishery has < 3 vessels to flag but delete reference fishery and assign those to nearest neighbors**

5. `scp` the whole thing back to local so have cluster results and original code.  

IMPORTANT: After analysis finalized, should re-run results with 2012, also with including all vessels, or removing more vessels. To see how robust results are.
