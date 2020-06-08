library(trajectories)
library(tidyverse)
library(sp)
library(sf)
library(spacetime)


ec <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)

ec$time <- gsub("T", " ", ec$time)
#ec$time <- as.POSIXct(ec$time)
ec$time <- ymd_hms(ec$time)

ec <- st_as_sf(ec, wkt = "geometry")

to_line <- function(tr) st_cast(st_combine(tr), "LINESTRING") %>% .[[1]]

ec.nest <- ec %>% group_by(track.id) %>% nest


tracks <- ec.nest %>% pull(data) %>% map(to_line) %>% st_sfc(crs = 4326)

ec.trj <- ec.nest %>% st_sf(geometry = tracks)

#Choose one track to make testing easier
track1 <- ec.trj[2,]
track1 <- track1 %>% unnest

spdf <- as_Spatial(track1)

space<-STIDF(geometry(spdf), spdf$time, spdf@data)
trajformatted <- Track(space)

