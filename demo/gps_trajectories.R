setwd('/home/john/Downloads/')
# Dataset used from:
# https://archive.ics.uci.edu/ml/datasets/GPS+Trajectories

gps_trajectories <- read.csv("go_track_trackspoints.csv",header = TRUE, check.names = TRUE)

sfgps_trajectories = geodata_to_sf(gps_trajectories, "track_id", "longitude", "latitude")

#Remove singular points since they are not trajectories
for(i in 1:nrow(sfgps_trajectories)){
  if(length(unlist(sfgps_trajectories$geometry[i])) <= 2){
    sfgps_trajectories = sfgps_trajectories[-i,]
  }
}

#Interactive visualization of trajectories
library(mapview)
mapview(sfgps_trajectories$geometry)

