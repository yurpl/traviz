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
time = t0 + cumsum(runif(n) * 60)
crs = CRS("+proj=longlat +datum=WGS84")
df = data.frame(SpatialPoints(cbind(x,y,id), crs), time, co2 = rnorm(n))
####

#Create sfTrack. Unique identifier is sp.id, longitude column is x, latitude column is y
df = as.data.frame(df)
sftrack1 = sfTrack(df, "id", "x", "y")
sftrack1@id

check_class = function(obj, class) {
  stopifnot(is(as(obj, class), class))
  cat("object is of desired class")
}

#Check coercion
check_class(as(sftrack1, 'data.frame'), 'data.frame')
check_class(as(sftrack1, 'Track'), 'Track')

