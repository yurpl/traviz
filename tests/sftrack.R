#Load required libraries

library(traviz)
library(raster)
library(sp)

t0 = as.POSIXct(as.Date("2013-09-30", tz = "CET"))
set.seed(13531) # make sure rbind generates identical sequences on reproduction

# Person A, track 1.

x = c(7, 6, 5, 5, 4, 3, 3)
y = c(7, 7, 6, 5, 5, 6, 7)
n = length(x)
t = t0 + cumsum(runif(n) * 60)
crs = CRS("+proj=longlat +datum=WGS84")
stidf = STIDF(SpatialPoints(cbind(x, y), crs), t, data.frame(co2 = rnorm(n)))

