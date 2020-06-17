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

as.sf.Tracks <- function(df){
  if(any(sapply(df, is.list))){
    df <- df %>% unnest
  }
  df$time <- as.POSIXct(df$time)
  df <- as_Spatial(df)
  df@proj4string = CRS("+proj=longlat +datum=WGS84")
  stidf <- STIDF(geometry(df), df$time, df@data)
  tracks = Track(stidf)
  return(tracks)
}

t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
t$time <- gsub("T", " ", t$time)
t <- geodata_to_sf(t, "track.id")

t <- t[1:10,]


test_track1 <- as.sf.Tracks(t[6,])
test_track2 <- as.sf.Tracks(t[4,])



#' Rasterize track data
#'
#' @param track track
#' @param value value wanted to raster with
#' @param resolution desired resolution
#' @return rasterized object
raster_track <- function(track, value, resolution){
  r <- rasterize(track@sp, raster(track@sp, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = resolution), track@data[[value]])
  crs(r) <- "+proj=longlat +datum=WGS84"
  return(r)
}


#' sf trajectory data frame to raster with selected properties to rasterize
#'
#' @param df Trajectory data frame in sf format to rasterize
#' @param data Data values wanted to rasterize
#' @param resolution Level of resolution
#' @param from Optional parameter from in as.POSIXct format to aggregate data from
#' @param to Optional parameter to in as.POSIXct format to aggregate to
#' @return rasterized object
#' @example trajectories <- geodata_to_sf(trajectories, "track.id")
#'
#' plot(sf_to_rasterize(track1, "Speed.value", .01))
#'
sf_to_rasterize <- function(df, data, resolution, from, to){
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

#' sf to stars raster
#'
#' @param df Trajectory data frame in sf format
#' @param value Data measurements to rasterize
#' @return stars object
sf_to_raster_stars <- function(df, value){
  return(st_rasterize(df[value]))
}

#' Aggregate raster to region of interest
#'
#' @param rasterized rasterized object
#' @param xmin
#' @param xmax
#' @param ymin
#' @param ymax
#' @return Cropped raster to ROI
#' @example plot(aggregate_raster_region(sf_to_raster(track1, "CO2.value", .0006), 7.63, 7.64, 51.954, 51.96))

aggregate_raster_region <- function(raster, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL){
  stopifnot(!missing(xmin), !missing(xmax), !missing(ymin), !missing(ymax))
  cropped <- crop(raster, extent((c(xmin, xmax, ymin, ymax))))
  crs(cropped) = "+proj=longlat +datum=WGS84"
  return(cropped)
}

#' Aggregate sf data frame to region of interest
#'
#' @param df sf data frame of trajectories with geometry column
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return Aggregated data frame
#' @example aggregate_sf_roi(t, 7.6, 7.7, 52, 52.3)

aggregate_sf_roi <- function(df, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  bb <- st_bbox(df)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  st_filter(df, st_as_sfc(bb), .predicate = st_within)
}

#' Interpolate raster using inverse distance weighted interpolation
#'
#' @param df data frame
#' @param measurement measurement to rasterize off
#' @param resolution desired resolution
#' @return Interpolated raster layer

#TODO: Make more elegant
idwi_raster <- function(df, measurement, resolution){
  df <- df %>% filter(!is.na(.data[[measurement]]))
  gs <- gstat(formula = as.formula(paste(measurement, paste(1, collapse = "+"), sep = " ~ ")), data = df)
  raster <- raster(df, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = resolution)
  i <- interpolate(raster, gs)
  r <- rasterize(df, raster, df[[measurement]])
  crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  interpolated = mask(i,r)
  crs(interpolated) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  return(interpolated)
}

#' Find density of intersections by returning raster layer of intersections
#'
#' @param df sf data frame
#' @param resolution desired resolution
#' @return rasterized intersections

find_intersections_density <- function(df, resolution){
  intersections <-st_intersection(df) %>% st_cast("POINT")
  return(rasterize(intersections, raster(intersections, res=resolution), intersections$n.overlaps))
}

#' Cluster trajectories
#'
#' @param trajectories trajectories data frame in sf format
#' @param num_clusters desired number of clusters
#' @return Returns clustered trajectories data frame

cluster_traj <- function(trajectories, num_clusters){
  trajectories <- st_transform(trajectories, crs = "+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
  clusters <- hclust(as.dist(st_distance(trajectories, which = "Frechet")))
  trajectories$cluster = as.factor(cutree(clusters, num_clusters))
  return((trajectories[,"cluster"]))
}
mapview::mapview(cluster_traj(test_reg, 10))

#' Plot kernel density heat map of trajectories
#'
#' @param trajectories trajectories dataframe
#' @return kernel density heatmap
#'
traj_heatmap <- function(trajectories){
  lines <- as_Spatial(trajectories$geometry)
  lines <- spTransform(lines, CRS("+proj=utm +zone=15 +ellps=WGS84"))
  linepsp <-maptools::as.psp.SpatialLines(lines, as.owin(st_bbox(lines)))
  plot(density(linepsp))
  plot(linepsp, add=TRUE)
}
kd_heatmap(ec.trj)

#' Plot kernel density heat map of trajectory measurements
#' @param trajectories trajectories data frame
#' @param value value desired to make heat map off
#' @param resolution desired resolution
#' @return plot of density heat map

density_heatmap <- function(trajectories, value, resolution){
  test_rast <- sf_to_raster(trajectories, value, resolution)
  rast_points <- data.frame(rasterToPoints(test_rast))
  rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
  rast_points <- as_Spatial(rast_points)
  rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
  plot(density(rast_points, at="pixels", weights = rast_points$marks))
  plot(rast_points, add=TRUE)
}
density_heatmap(trackcol_agg, "Speed.value", .0005)



#' Plot quadrat intensity of points of a trajectory
#' @param trajectories sf trajectories data frame
#' @return plot of density heat map
traj_quadrat <- function(trajectories){
  spdf <- as_Spatial(trajectories)
  spdf.ppp <- maptools::as.ppp.SpatialPointsDataFrame(spdf)
  plot(intensity(quadratcount(spdf.ppp), image=TRUE))
  plot(trajectories$geometry, add=TRUE)
}


#' Plot space time cube of sf trajectory
#' @param df data frame sf trajectory
#' @return space time cube

sfcube <- function(df){
  anglr::plot3d(df)
}


#' Plot trajectories using ggplot2
#' @param trajectories trajectories data frame
#' @param value value to base scale off
#' @return ggplot2 of trajectories

plot_traj <- function(trajectories, value){
  p <- ggplot() +
    geom_sf() +
    geom_sf(data = trajectories, aes(color = .data[[value]]))+
    scale_color_gradient(low = "yellow", high = "red", na.value = NA)
  p
}
plot_traj(track1, "CO2.value")
