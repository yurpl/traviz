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

#Create sfTrack
track1 = sfTrack(sfgps_trajectories[1,])

#Visualization
plot(track1)
pv_stcube(track1, map=T)

#Point data analysis
sfgps_trajectories_points = sfgps_trajectories %>% unnest
subset_trajectories_points = sfgps_trajectories_points[1:1000,]

plot_hour_density(subset_trajectories_points)
plot_day_density(subset_trajectories_points)


