Metier_efforts
========================================================
author: Emma
date: 2014-04-19




Goal
========================================================
left:60%


MCA on entire logbook contents, CLARA on entire dataset

relevant effort variables to cluster from fish ticket dataset

+ `month`
+ `grid` (finer than `grgroup`)
+ `catch profile`

***
might be useful: unsure what `pargrp` is, from [PacFin](http://pacfin.psmfc.org/pacfin_pub/table_cols.php)
> Participation group; see par-group in table cl.]; vesseltype [Vessel type (see vid-type in table cl)])

GRID: Issue of unspecified gears
========================================================
left:75%

+ `OTH` = other known gear
+ `OHL` = other hook and line gear
+ `ONT` = other net gear
+ `OPT` = other pot gear
+ `USP` = unknown or unspecified
+ `ODG` = other dredge gear

See [here](http://pacfin.psmfc.org/pacfin_pub/data_rpts_pub/code_lists/gr_tree.txt) for reference
***
>Clear that I can remove `USP`, but what about others? 

Distribution of gear types
========================================================
![plot of chunk unnamed-chunk-2](effort-figure/unnamed-chunk-2.png) 

Mostly made up of `CPT` (crab pots), `LGL` (long line/set line), `MDT` (midwater trawls), `RLT` (roller trawls), `TRL` (trolls). 

But `OHL` (other hook and line) and `OTL` (other known gear) are quite substantial 

Can I tell what OTH and OHL are?
========================================================
What is `OHL` catching? 

[subset to species present on trips > 100x]

![plot of chunk unnamed-chunk-3](effort-figure/unnamed-chunk-3.png) 

Mostly rockfish (some lingcod)

How about OTH?
========================================================
[subset to species present on trips > 100x]

![plot of chunk unnamed-chunk-4](effort-figure/unnamed-chunk-4.png) 


Mostly claims, cockles, lots of razor clams, and some red sea urchins. Will remove for subsequent analysis 

Subset data for MCA
========================================================

```r
unspecifed <- c("OTH","OHL","ONT","OPT","USP","ODG")
mca.data <- subset(ftl_clust, !(grid %in% unspecifed), select=c("month","grid","catch_profile")) 
```


Run MCA
========================================================

```r
mca.analysis <- mjca(mca.data)
plot(mca.analysis,cex=2)
```

<img src="effort-figure/unnamed-chunk-6.png" title="plot of chunk unnamed-chunk-6" alt="plot of chunk unnamed-chunk-6" style="display: block; margin: auto;" />


========================================================
According to Jonas, should scale `values3` to 100% and take only the components that make up 80% of the variance. Not sure where the component-scaled type data exist in the `mca.analysis` object. Also I don't have that many column variables now (only 3), so am just going to go for the clustering.


```r
summary(mca.analysis)$scree
```

```
                  values2 values3
 [1,]  1 0.421578    25.9    25.9
 [2,]  2 0.250770    15.4    41.2
 [3,]  3 0.191187    11.7    53.0
 [4,]  4 0.162577    10.0    62.9
 [5,]  5 0.126548     7.8    70.7
 [6,]  6 0.053557     3.3    74.0
 [7,]  7 0.036849     2.3    76.2
 [8,]  8 0.030384     1.9    78.1
 [9,]  9 0.017905     1.1    79.2
[10,] 10 0.014943     0.9    80.1
[11,] 11 0.008120     0.5    80.6
[12,] 12 0.003873     0.2    80.9
[13,] 13 0.002013     0.1    81.0
[14,] 14 0.001214     0.1    81.0
[15,] 15 0.000638     0.0    81.1
[16,] 16 0.000360     0.0    81.1
[17,] 17 0.000212     0.0    81.1
[18,] 18 0.000065     0.0    81.1
[19,] 19 0.000047     0.0    81.1
[20,] 20 0.000020     0.0    81.1
[21,] 21 0.000012     0.0    81.1
[22,] 22 0.000002     0.0    81.1
[23,] 23 0.000001     0.0    81.1
[24,] 24 0.000000     0.0    81.1
```


CLARA on data
========================================================
clust.ef <- 
