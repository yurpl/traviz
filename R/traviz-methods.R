#' Convert trajectory data frame in  or lat long format to sf
#'
#' @param df A trajectory data frame with a geometry column or in lat long format
#' @param identifier Unique identifier to group data frame by
#' @return A nested data frame in sf format


geodata_to_sf <- function(df, identifier){
  if(missing(identifier)){
    stop("Missing unique identifier to group geometries by")
  }
  if(inherits(df$time, 'POSIXct')){
    df$time = as.POSIXct(df$time)
  }
  if("geometry" %in% colnames(df) && class(df$geometry) != "sfc" && class(df$geometry) != "sfc_POINT" ){
    df <- st_as_sf(df, wkt = "geometry")
  }
  else if("lat" %in% colnames(df) && "long" %in% colnames(df)){
    df <- st_as_sf(coords = c("long", "lat"), crs = 4326)
  }
  else{
    df <- st_as_sf(df, df$geometry)
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
#' @param df Trajectory data frame in sf format to rasterize or sfTrack or sfTracks
#' @param data Data values wanted to rasterize
#' @param resolution Level of resolution
#' @param from Optional parameter from in as.POSIXct format to aggregate data from
#' @param to Optional parameter to in as.POSIXct format to aggregate to
#' @return rasterized object
sf_to_rasterize <- function(df, data, resolution, from, to){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  if(missing(from) && missing(to)){
    df <- df %>% filter(!is.na(.data[[data]]))
    st_crs(df) <- 4326
    r <- rasterize(df, raster(df, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = resolution), df[[data]])
    crs(r) <- "+proj=longlat +datum=WGS84"
    return(r)
  }
  else{
    library(tibbletime)
    df <- df %>%
      arrange(time) %>%
      as_tbl_time(index = time) %>%
      filter_time(from ~ to)
    df <- st_as_sf(df)

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
#' @param xmin min x value
#' @param xmax max x calue
#' @param ymin min y value
#' @param ymax max y value
#' @return Cropped raster to ROI

aggregate_raster_region <- function(raster, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL){
  stopifnot(!missing(xmin), !missing(xmax), !missing(ymin), !missing(ymax))
  cropped <- crop(raster, extent((c(xmin, xmax, ymin, ymax))))
  crs(cropped) = "+proj=longlat +datum=WGS84"
  return(cropped)
}

#' Aggregate sf data frame to region of interest
#'
#' @param df sf data frame of trajectories with geometry column or sfTrack or sfTracks
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return Aggregated data frame

aggregate_sf_roi <- function(df, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  bb <- st_bbox(df)
  if (!is.null(xmin)) bb["xmin"] <- xmin
  if (!is.null(xmax)) bb["xmax"] <- xmax
  if (!is.null(ymin)) bb["ymin"] <- ymin
  if (!is.null(ymax)) bb["ymax"] <- ymax
  st_filter(df, st_as_sfc(bb), .predicate = st_within)
}

#' Interpolate raster using inverse distance weighted interpolation
#'
#' @param df data frame or sfTrack or sfTracks
#' @param measurement measurement to rasterize off
#' @param resolution desired resolution
#' @return Interpolated raster layer

#TODO: Make more elegant
idwi_raster <- function(df, measurement, resolution){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
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
#' @param df sf data frame or sfTrack or sfTracks
#' @param resolution desired resolution
#' @return rasterized intersections

find_intersections_density <- function(df, resolution){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  intersections <-sf::st_intersection(df) %>% st_cast("POINT")
  return(rasterize(intersections, raster(intersections, res=resolution), intersections$n.overlaps))
}

#' Cluster trajectories
#'
#' @param df trajectories data frame in sf format or sfTrack or sfTracks
#' @param num_clusters desired number of clusters
#' @return Returns clustered trajectories data frame

cluster_traj <- function(df, num_clusters){
  df <- st_transform(df, crs = "+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
  clusters <- hclust(as.dist(sf::st_distance(df, which = "Frechet")))
  df$cluster = as.factor(cutree(clusters, num_clusters))
  return((df[,"cluster"]))
}

#' Plot kernel density heat map of trajectories
#'
#' @param df trajectories dataframe or sfTrack or sfTracks
#' @return kernel density heatmap
#'
traj_heatmap <- function(df){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  lines <- as_Spatial(df$geometry)
  lines <- spTransform(lines, CRS("+proj=utm +zone=15 +ellps=WGS84"))
  linepsp <-maptools::as.psp.SpatialLines(lines, as.owin(st_bbox(lines)))
  plot(density(linepsp), main = "Heatmap plot")
  plot(linepsp, add=TRUE)
}
#kd_heatmap(ec.trj)

#' Plot kernel density heat map of trajectory measurements
#' @param df trajectories data frame or sfTrack or sfTracks
#' @param value value desired to make heat map off
#' @param resolution desired resolution
#' @param date optional parameter to create a day heatmap with 6 plots and 4 hour intervals
#' @return plot of density heat map

density_heatmap <- function(df, value, resolution, date){
  # if(is(df, 'sfTracks') || is(df, 'sfTrack')){
  #   df <- as(df, "data.frame")
  #   df <- st_as_sf(df)
  # }
  if(missing(date)){
    test_rast <- sf_to_rasterize(df, value, resolution)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")
    plot(rast_points, add=TRUE)
  }
  else{
    par(mfrow=c(2,3))
    test_rast <- sf_to_rasterize(df, value, resolution, from = date, to = date + 4*60*60)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")

    test_rast <- sf_to_rasterize(df, value, resolution, from = date + (4*60*60), to = date + (8*60*60))
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")

    test_rast <- sf_to_rasterize(df, value, resolution, from = date + 8*60*60, to = date + 12*60*60)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")


    test_rast <- sf_to_rasterize(df, value, resolution, from = date + 12*60*60, to = date + 16*60*60)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")

    test_rast <- sf_to_rasterize(df, value, resolution, from = date + 16*60*60, to = date + 20*60*60)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")

    test_rast <- sf_to_rasterize(df, value, resolution, from = date + 20*60*60, to = date + 24*60*60)
    rast_points <- data.frame(rasterToPoints(test_rast))
    rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
    rast_points <- as_Spatial(rast_points)
    rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
    plot(density(rast_points, at="pixels", weights = rast_points$marks), main = "Heatmap plot")
  }
}


#' Plot quadrat intensity of points of a trajectory
#' @param df sf trajectories data frame
#' @return plot of density heat map
traj_quadrat <- function(df){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  spdf <- as_Spatial(df)
  spdf.ppp <- maptools::as.ppp.SpatialPointsDataFrame(spdf)
  plot(intensity(quadratcount(spdf.ppp), image=TRUE), main = "Quadrat plot")
  plot(df$geometry, add=TRUE)
}


#' Plot space time cube of sf trajectory
#' @param df data frame sf trajectory
#' @return space time cube

sfcube <- function(df){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  anglr::plot3d(df)
}


#' Plot trajectories using ggplot2
#' @param trajectories trajectories data frame
#' @param value value to base scale off
#' @return ggplot2 of trajectories

plot_traj <- function(df, value){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  p <- ggplot() +
    geom_sf() +
    geom_sf(data = df, aes(color = .data[[value]]))+
    scale_color_gradient(low = "yellow", high = "red", na.value = NA)
  p
}

#' Get XY coordinates from sf object (taken from jmlondon at https://github.com/r-spatial/sf/issues/231)
#' @param x data frame in sf format
#' @param names names to name XY columns
#' @return data frame with XY columns
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


#' Animate single trajectory using movevis (BEWARE OF MEMORY/RENDERING PROBLEMS)
#' @param trajectory singular trajectory data frame
#' @param res temporal resolution (i.e. 5 = 5 mins)
#' @param units units for temporal resolution (minutes at default)
#' @param filename filename for output GIF
#' @return animation of trajectory in GIF

animate_single_track <- function(trajectory, res, filename = "trajectory.gif", unit = "min"){
  if(is(trajectory, 'sfTrack')){
    trajectory <- as(trajectory, "data.frame")
    trajectory <- st_as_sf(trajectory)
  }
  trajectory <- sfc_as_cols(trajectory)
  move <- df2move(trajectory, proj ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", x='x', y='y', time='time' )
  m <- align_move(move, res = res, unit = unit)
  frames <- frames_spatial(m, path_colours = c("red"), map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
    add_labels(x = "Longitude", y = "Latitude") %>%
    add_northarrow() %>%
    add_scalebar() %>%
    add_timestamps(m, type = "label") %>%
    add_progress()
  animate_frames(frames, out_file = filename)
}

#' Coerce nested sf data frame to sfTracks
#'
#' @param df data frame to coerce
#' @return sfTracks object
df_to_sfTracks <- function(df){
  nest_ls <- list()
  for(i in 1:nrow(df)){
    track <- sfTrack(df[i,])
    nest_ls <- append(nest_ls, track)
  }
  return(sfTracks(nest_ls))
}

#' Polygonal raster pattern analysis
#'
#' @param df trajectories data frame in sf format
#' @param value value to make raster poly
#' @param res resolution
#' @return returns polygon dataframe
ppa_polygons <- function(df, value, res){
  if(is(df, 'sfTracks') || is(df, 'sfTrack')){
    df <- as(df, "data.frame")
    df <- st_as_sf(df)
  }
  poly_points <- rasterToPolygons(sf_to_rasterize(df, value, res))
  poly_points <- st_as_sf(poly_points)

  return(poly_points)
}

#' Getis-ord hotspot analysis
#'
#' @param poly_points polygon points from ppa_polygons function
#' @return heatmap

gi_hotspot<- function(poly_points){
  if(class(poly_points) != "sfc_POLYGON") {warning("Function requires pologyon points. Use ppa_polygons function")}
  library(spdep)
  G <- localG(x = poly_points$layer, listw = nb2listw(poly2nb(poly_points), zero.policy = TRUE))
  poly_points$g <- as.numeric(G)
  ggplot(poly_points) +
    geom_sf(aes(fill = g)) +
    scale_fill_viridis_c(option = "magma", name = "Gi* Statistic") +
    theme_void() +
    ggtitle("Getis-Ord Point Analysis")
  }


#' Aggregate sfTrack by time
#'
#' @param sftrack sfTrack
#' @param from from in posixct format
#' @param to to in posxict format
#' @return Returns aggregated sfTrack

aggregate_sft_time <- function(sftrack, from, to){
  if(from < min(sftrack@time) | to > max(sftrack@time)){
    warning("Time period nonexistant in track")
  }
  library(tibbletime)
  df <- as(sftrack, "data.frame")
  df <- st_as_sf(df)
  df <- df %>%
    as_tbl_time(index = time) %>%
    filter_time(from ~ to)
  return(sfTrack(df, "track.id"))
}

#' Aggregate dataframe by day (i.e. all trajectories on Monday)
#'
#' @param traj trajectory df
#' @param day day to aggregate by (1 = Sunday, 2 = Monday, ... , 7 = Saturday)
#' @return aggregated data frame

aggregate_day <- function(traj, day){
  library(lubridate)
  return(traj[which(wday(traj$time) == day), ])
}

#' Visualize density in region of interest by day of week
#' @import lubridate
#' @param df trajectories data frame
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return plot of aggregated values

plot_day_density <- function(df, xmin, xmax, ymin, ymax){
  if (!missing(xmin) && !missing(xmax) && !missing(ymin) && !missing(ymax)) {df <- aggregate_sf_roi(df, xmin, xmax, ymin, ymax)}

  df <- sfc_as_cols(df)
  df$weekday = weekdays(as.Date(as.POSIXct(df$time), format="%m/%d/%Y"))
  df$weekday = factor(df$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <-df[order(df$weekday),]

  p <- ggplot(df) + geom_sf() + stat_density2d(data = df, aes(x,y, fill = ..level.., alpha=..level..), size = 1, bins = 20, alpha=0.25,  geom = "polygon") + scale_fill_gradient(low="blue", high = "orange")+ facet_wrap(~weekday)  + ggtitle("Aggregation by weekday")
  return(p)

}

#' Visualize density in region of interest by time of day
#' @import lubridate
#' @param df trajectories data frame
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return plot of aggregated values

plot_hour_density <- function(df, xmin, xmax, ymin, ymax){
  if (!missing(xmin) && !missing(xmax) && !missing(ymin) && !missing(ymax)) {df <- aggregate_sf_roi(df, xmin, xmax, ymin, ymax)}

  df <- sfc_as_cols(df)
  df$time <- lubridate::hour(df$time)

  p <- ggplot(df) + geom_sf() + stat_density2d(data = df, aes(x,y, fill = ..level.., alpha=..level..), size = 1, bins = 20, alpha=0.25,  geom = "polygon") + scale_fill_gradient(low="blue", high = "orange")+ facet_wrap(~time) + ggtitle("Aggregation by hour")
  return(p)

}

#' Plot values by weekday
#' @import lubridate
#' @param df trajectories data frame
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return plot of aggregated values

plot_day <- function(df, value, xmin, xmax, ymin, ymax){
  if (!missing(xmin) && !missing(xmax) && !missing(ymin) && !missing(ymax)) {df <- aggregate_sf_roi(df, xmin, xmax, ymin, ymax)}

  df$weekday = weekdays(as.Date(as.POSIXct(df$time), format="%m/%d/%Y"))
  df$weekday = factor(df$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <-df[order(df$weekday),]

  if(missing(value)){
    p <- ggplot(df) + geom_sf() + scale_fill_gradient(low="blue", high = "orange")+ facet_wrap(~weekday) + ggtitle("Aggregation by weekday")
    return(p)
  }
  else{
    p <- ggplot(df) + geom_sf(data = df, aes(color = .data[[value]]))+ scale_color_gradient(low = "yellow", high = "red", na.value = NA) + facet_wrap(~weekday) + ggtitle("Aggregation by weekday")
    return(p)
  }
}

#' Plot values by hour
#' @import lubridate
#' @param df trajectories data frame
#' @param xmin min x
#' @param xmax max x
#' @param ymin min y
#' @param ymax max y
#' @return plot of aggregated values

plot_hour <- function(df, value, xmin, xmax, ymin, ymax){
  if (!missing(xmin) && !missing(xmax) && !missing(ymin) && !missing(ymax)) {df <- aggregate_sf_roi(df, xmin, xmax, ymin, ymax)}

  df$time <- lubridate::hour(df$time)

  if(missing(value)){
    p <- ggplot(df) + geom_sf() + facet_wrap(~time) + ggtitle("Aggregation by hour")
    return(p)
  }
  else{
    p <- ggplot(df) + geom_sf(aes(color = .data[[value]]))+ scale_color_gradient(low = "yellow", high = "red") + facet_wrap(~time) + ggtitle("Aggregation by hour")
    return(p)
  }
}

#' Plot time series of track
#' @param sft sfTrack
#' @param value y value
#' @return ggplot time series

sft.plot_ts <- function(sft, value){
  p <- ggplot(sft@data) + geom_point(aes(x=time, y=.data[[value]])) + ggtitle("Track value time series")
  return(p)
}

#' Plot violin plot of sfTracks value
#' @param sfts sfTracks
#' @param value y calue
#' @return violin plot

sfts.plot_violin <- function(sfts, value){
  df <- as(sfts, 'data.frame')
  df$track.id <- as.factor(df$track.id)
  p <- ggplot(data = df, aes(x=track.id, y=.data[[value]], fill=track.id)) + geom_violin()
  return(p)
}

#' Calendar heatmap of trajectories data frame
#' @param df data frame of trajectories
#' @param value value for heatmap
#' @return calendar heatmap

calendar_heatmap <- function(df, value){
  library(lubridate)
  df$weekday = weekdays(as.Date(df$time, format="%m/%d/%Y"))
  df$weekday = factor(df$weekday, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  df <-df[order(df$weekday),]

  df$hour == factor(hour(ymd_hms(df$time)))

  p <- ggplot(df, aes(weekday, hour, fill = Speed.value)) +
    geom_tile(colour = "white") + facet_grid(~weekday) + scale_fill_gradient(low="red", high="green")
  return(p)
}
