# Home range estimation

Using the `adehabitatHR` to estimate each fishermen's 'home range,' in the observer dataset, as in: every place they've ever fished. There area couple of methods I could use to estimate home ranges

1. the minimum convex polygon (Mohr, 1947)
2. Kernel methods
  a. Classical kernal method (Worton, 1989)
  b. Brownian bridge kernel method (Bullard, 1999; Horne et al. 2007)
  c. Biased random bridge kernel method ("movement-based kernel esitmation") (Benhamou & Cornelis, 2010; Benhamou, 2011)
  d. Product kernel algorithm (Keating & Cherry, 2009)




## Minimum convex polygons (MCPs)
The MCP is the calculation of the smallest convex polygon that will enclose all the relocations of the animal (in this case the vessel). The function `mcp` allows MCP estimation. Normally with home-range estimation some percentange of the most extreme points are omitted on the assumption that some subset of relocations are not 'normal' behavior. However with observer data, can be fairly sure that all recorded behavior is correct, although there may be some error on the part of the observer recording. So we'll remove 5% of the extreme points


```r
cp <- mcp(d1[, 1], percent = 95)
plot(cp)
plot(d1, add = TRUE)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

```r

# getting the size of the home range requires coercing cp into a dataframe
# not sure the units, assume it's degrees squared? example said it was
# hectares because original units were in meters...
as.data.frame(cp)
```

```
##   id      area
## 1  1 0.0003451
```

To test the amount of extreme points to leave out can use function `mcp.area` which will compute the home-range for various choices of extreme locations to be excluded


```r
hrs <- mcp.area(d1[, 1], percent = seq(50, 100, by = 5))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


We're looking for the assymptote, but that seems to mean dropping 30% of the points. And 30% of points means that the area wasn't really abnormally used. So probably should keep all points in. 

```r
cp <- mcp(d1[, 1], percent = 100)
plot(cp)
plot(d1, add = TRUE)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


## Utilization distributions
Under this model the animals use of space can be described by a bivariate probability distribution function, the $UD$, which gives the probability density to relocate the animal at any place according to the coordinates (x, y) of this place. The study of the space use by an animal could be considered the properties of the utilization distribution. The issue is therefore to estiamte the utilization distribution from the relocation data. The kernel method has been the most widely used. The math can be found in the `adehabitatHR` vignette, but the smoothing parameter $h$ controls the "width" of the kernel functions placed over each point. The correct $h$ has been much researched and debated, but often visual choice is recommended (see the vignette for more). 

In the `adehabitatHR` package the argument `h` controls the value of the smoothing parameter. If `h = "href"` the "reference" bandwidth is sued in the estimation (which is often two wide for animals going to discrete patches close together); if `h = "LSCV"` the "LSCV" bandwidth is used in the estimation. It's also possible to pass a numeric value to the smoothing parameter. 

I need to spend much more time thinking about this paramter value. 


```r
kud <- kernelUD(d1[, 1], h = "href")
image(kud)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 


To estimate the home range from the UD, we just deduce the minimum area on which the probability to relocate an animal is equal to a specified value. For example, the 95% home range corresponds to the smallest area on which the probability to relocate the animal is equal to $0.95$. The `getvolumeUD` and `getverticeshr` provide utilities for home range estimation. 


```r
# To get the 90% home range from the UD estimated using href
homerange <- getverticeshr(kud)
plot(homerange, col = "dodgerblue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 


The function `getvolumeUD` may be useful to estimate home range in raster mode

```r
vud <- getvolumeUD(kud)
image(vud)
xyzv <- as.image.SpatialGridDataFrame(vud[[1]])
contour(xyzv, add = TRUE)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 



Using the function `clusthr` which implements the single-linkage clustering algorithm,[^1] can generate polygons of home range. 

[^1]: which I don't understand yet

```r
clu <- clusthr(d1)
plot(clu)
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 


