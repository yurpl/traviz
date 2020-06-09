library(tidyverse)
library(sf)
library(trajectories)
library(spacetime)
library(raster)
#' Convert trajectory data frame in  or lat long format to sf
#'
#' @param df A trajectory data frame with a geometry column or in lat long format
#' @param identifier Unique identifier to group data frame by
#' @return A nested data frame in sf format
#' @example
#' trajectories <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
#' trajectories <- geodata_to_sf(trajectories, "track.id")

geodata_to_sf <- function(df, identifier){
  if(missing(identifier)){
    stop("Missing unique identifier to group geometries by")
  }

  if("geometry" %in% colnames(df)){
    df <- st_as_sf(df, wkt = "geometry")
  }
  else if("lat" %in% colnames(df) && "long" %in% colnames(df)){
    df <- st_as_sf(coords = c("long", "lat"), crs = 4326)
  }
  to_line <- function(tr) st_cast(st_combine(tr), "LINESTRING") %>% .[[1]]

  df.nest <- df %>% group_by(.data[[identifier]]) %>% nest
  tracks <- df.nest %>% pull(data) %>% map(to_line) %>% st_sfc(crs = 4326)
  df.traj = df.nest %>% st_sf(geometry = tracks)
  st_set_crs(df.traj, 4326)
  invisible(df.traj)
}

#' Convert sf data to Track
#'
#' @param df Trajectory data frame in sf and sftime format to be converted to Track
#' @return Track
#' @example spdf <- as_Spatial(df)
#' space<-STIDF(geometry(spdf), spdf$time, spdf@data)
# 'trajformatted <- Track(space)

sf_to_track <- function(df){
  #if(!('time' %in% colnames(df))){
    #stop("No timestamp column")
  #}
  df$time <- as.POSIXct(df$time)
  df <- as_Spatial(df)
  df@proj4string = CRS("+proj=longlat +datum=WGS84")
  stidf <- STIDF(geometry(df), df$time, df@data)
  return(Track(stidf))
}

t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
t$time <- gsub("T", " ", t$time)
t <- geodata_to_sf(t, "track.id")
#t <- t[2,]
t <- t %>% unnest
plot(sf_to_track(t))

#' sf data frame to raster with selected properties to rasterize
#'
#' @param df Trajectory data frame in sf format to rasterize
#' @param data Data values wanted to rasterize
#' @param resolution Level of resolution
#' @param from Optional parameter from in as.POSIXct format to aggregate data from
#' @param to Optional parameter to in as.POSIXct format to aggregate to
#' @return rasterized object
#' @example trajectories <- geodata_to_sf(trajectories, "track.id")
#' rasterized <- sf_to_raster(t, "CO2.value", .001)
#' plot(rasterized)
#'
sf_to_raster <- function(df, data, resolution, from, to){
  if(missing(from) && missing(to)){
    df <- df %>% filter(!is.na(.data[[data]]))
    r <- rasterize(df, raster(df, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = resolution), df[[data]])
    crs(r) <- "+proj=longlat +datum=WGS84"
    return(r)
  }
  else{
    df <- df %>%
      filter(!is.na(.data[[data]])) %>%
      filter(time >= from & time <= to)
    r <- rasterize(df, raster(df, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = resolution), df[[data]])
    crs(r) <- "+proj=longlat +datum=WGS84"
    return(r)
  }
}
plot(sf_to_raster(t, "CO2.value", .005, ""))
plot(sf_to_raster(t, "Speed.value", .01, as.POSIXct("2019-12-24 15:25:33"), as.POSIXct("2020-01-05 15:56:54")))

#' Aggregate raster to region of interest
#'
#' @param rasterized rasterized object
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @return Cropped raster to ROI
#' @example plot(aggregate_raster_roi(sf_to_raster(t, "CO2.value", .0006), 7.63, 7.64, 51.954, 51.96))

aggregate_raster_roi <- function(raster, xmin, xmax, ymin, ymax){
  cropped <- crop(raster, extent((c(xmin, xmax, ymin, ymax))))
  crs(cropped) = "+proj=longlat +datum=WGS84"
  return(cropped)
}




