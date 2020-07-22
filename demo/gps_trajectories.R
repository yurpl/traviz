setwd('/home/john/Downloads/')
# Dataset used from:
# https://archive.ics.uci.edu/ml/datasets/GPS+Trajectories

gps_trajectories <- read.csv("go_track_trackspoints.csv",header = TRUE, check.names = TRUE)

sfgps_trajectories = geodata_to_sf(gps_trajectories, "track_id", "longitude", "latitude")
#TODO: Remove singular points to prevent errors
