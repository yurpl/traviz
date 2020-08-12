#Load required libraries
#Test methodology followed from trajectories (https://github.com/edzer/trajectories/blob/master/tests/tracks.R)

library(traviz)
library(raster)
library(sp)


#Source: https://github.com/edzer/trajectories/blob/master/tests/tracks.R
t0 = as.POSIXct(as.Date("2013-09-30", tz = "CET"))
set.seed(13531) # make sure rbind generates identical sequences on reproduction

# Person A, track 1.
x = c(7, 6, 5, 5, 4, 3, 3)
y = c(7, 7, 6, 5, 5, 6, 7)
id = c(1, 1, 1, 1, 1, 1, 1)
n = length(x)
t = t0 + cumsum(runif(n) * 60)
crs = CRS("+proj=longlat +datum=WGS84")
stidf = STIDF(SpatialPoints(cbind(x, y, id), crs), t, data.frame(co2 = rnorm(n)))
####

#Create sfTrack. Unique identifier is sp.id, longitude column is x, latitude column is y
stidf = as.data.frame(stidf)
sftrack1 = sfTrack(stidf, "id", "x", "y")
sftrack1@id

#Check coercion
checkClass = function(list, class) {
  stopifnot(all(sapply(list, function(x) class(x)[1] == class)))
}



