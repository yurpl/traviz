transform.sfTrack <- function(x, crs = ""){
  x@line = sf::st_transform(x@line, crs)
  x@geometry = sf::st_transform(x@geometry, crs)
  return(x)
}

setMethod("transform.sfTrack", "sfTrack", transform.sfTrack)

setGeneric("sft_coordinates", function(sft, ...)
  standardGeneric("sft_coordinates"))

sft_coordinates.sfTrack <- function(sft) {return(as.data.frame(sf::st_coordinates(sft@geometry)))}

setMethod("sft_coordinates", "sfTrack", sft_coordinates.sfTrack)


sft_coordinates.sfTracks <- function(sft) {
  return(do.call(rbind, lapply(sft@tracks,
                        function(x) sft_coordinates(x))))
}
setMethod("sft_coordinates", "sfTracks", sft_coordinates.sfTracks)


setGeneric(
  name = "intersection",
  def = function(sft1, sft2, ...) standardGeneric("intersection")
)

#Intersection between two tracks
intersection.sfTrack <- function(sft1, sft2, zoom = FALSE){
  intersection <- sf::st_intersection(sft1@line, sft2@line)
  zoom_box <- st_bbox(intersection)
  if(is_empty(intersection)){
    warning("Trajectories do not intersect")
  }
  if(missing(zoom)){
    p <- ggplot() +
      geom_sf(data = sft1@line, aes(color = sft1@id)) +
      geom_sf(data = sft2@line, aes(color = sft2@id)) +
      geom_sf(data = intersection, size = 5, shape = 1) +
    labs(color = "Track ID")
    return(p)
  }
  else{
    p <- ggplot() +
      geom_sf(data = sft1@line, aes(color = sft1@id)) +
      geom_sf(data = sft2@line, aes(color = sft2@id)) +
      geom_sf(data = intersection, size = 5, shape = 1) +
      coord_sf(xlim = c(as.numeric(zoom_box[1]), as.numeric(zoom_box[3])), ylim = c(as.numeric(zoom_box[2]), as.numeric(zoom_box[4]))) +
      labs(color = "Track ID")
    return(p)
  }
}

setMethod("intersection", "sfTrack", intersection.sfTrack)

cluster.sfTracks <- function(sftc, num_clusters){
  trajectories <- as(sftc, "data.frame")
  trajectories.nest <- geodata_to_sf(trajectories, "track.id")
  clusters <- hclust(as.dist(st_distance(trajectories.nest$geometry, which = "Frechet")))
  trajectories.nest$cluster = as.factor(cutree(clusters, num_clusters))
  return((trajectories[,"cluster"]))
}
setMethod("cluster.sfTracks", "sfTrack", cluster.sfTracks)



setGeneric(
  name = "distance",
  def = function(sft1, sft2, ...) standardGeneric("distance")
)

distance.sfTrack <- function(sft1, sft2, which = ""){
  dist <- sf::st_distance(sft1@line, sft2@line, which = which)
  return(dist)
}

setMethod("distance", "sfTrack", distance.sfTrack)


print.sfTrack <- function(object){
  track = object
  cat("An object of class sfTrack \n");
  cat(paste0(nrow(as.data.frame(track@geometry)), "points"),"\n");
  cat(paste0("bbox:"),"\n");
  print(sf::st_bbox(track@geometry));
  cat(paste0("Time period: [",min(track@time),", ", max(track@time),"]"))
}
setMethod("show", "sfTrack", print.sfTrack)

print.sfTracks <- function(object){
  cat("An object of class sfTracks" ,"\n");
  cat(paste0(length(object@tracks)), "sftracks")
}
setMethod("show", "sfTracks", print.sfTracks)

#Coerce to Track
setAs("sfTrack", "Track",
      function(from){
        geometry <- sf::as_Spatial(from@geometry)
        geometry@proj4string = CRS("+proj=longlat +datum=WGS84")
        stidf <- STIDF(sp::geometry(geometry), from@time, from@data)
        track = Track(stidf)
        return(track)
      })

#Coerce to data frame
setAs("sfTrack", "data.frame",
      function(from){
        df <- as(from@data, "data.frame")
        df$track.id <- from@id
        return(df)
      }
)

setAs("sfTracks", "data.frame",
      function(from){
        l <- lapply(from@tracks, function(x) rbind(as(x, "data.frame"), NA))
        for(i in 1:length(l)){
          l[[i]]$track.id <- from@tracks[[i]]@id
        }
        d = do.call(rbind, l)
        rownames(d) <- NULL
        d
      }
      )

sft_bbox <- function(sft){
  return(sf::st_bbox(sft@line))
}

setMethod("sft_bbox", "sfTrack", sft_bbox)

cube.sfTrack <- function(x, ...){
  trajectories::stcube(as(x, "Track"))
}

setMethod("cube.sfTrack", "sfTrack", cube.sfTrack)


sft_length <- function(sft){
  return(sf::st_length(x@line))
}
setMethod("sft_length", "sfTrack", sft_length)

#Functions for OSM and 3d map support from trajectories package
#Authors: Benedikt Graeler and Edzer Pebesma
OSM = function(xlim, ylim, mapZoom, mapType, projection) {
  if (!requireNamespace("OpenStreetMap", quietly = TRUE))
    stop("package OpenStreetMap required")
  bboxSp <- SpatialPoints(rbind(c(xlim[1], ylim[2]),
                                c(xlim[2], ylim[1])))
  bboxSp@proj4string <- CRS(projection)
  bboxSp <- spTransform(bboxSp, CRS("+init=epsg:4326"))
  map = OpenStreetMap::openmap(upperLeft = bboxSp@coords[1,2:1],
                               lowerRight = bboxSp@coords[2,2:1], zoom = mapZoom, type = mapType)
  OpenStreetMap::openproj(x = map, projection = projection)
}

map3d = function(map, z, ...) {
  if (!requireNamespace("rgl", quietly = TRUE))
    stop("rgl required")
  if(length(map$tiles) != 1)
    stop("Pass single map tile only.")
  nx = map$tiles[[1]]$xres
  ny = map$tiles[[1]]$yres
  xmin = map$tiles[[1]]$bbox$p1[1]
  xmax = map$tiles[[1]]$bbox$p2[1]
  ymin = map$tiles[[1]]$bbox$p1[2]
  ymax = map$tiles[[1]]$bbox$p2[2]
  xc = seq(xmin, xmax, len = ny)
  yc = seq(ymin, ymax, len = nx)
  col = matrix(data = map$tiles[[1]]$colorData, nrow = ny, ncol = nx)
  m = matrix(data = z, nrow = ny, ncol = nx)

  rgl::surface3d(x = xc, y = yc, z = m, col = col, lit = FALSE, ...)
}

normalize = function(time, by = "week") {
  tn = as.numeric(time)

  switch(by,
         minute = (tn %% 60),
         hour = (tn %% 3600) / 60 , # decimal minute of the hour
         day = (tn %% (3600 * 24)) / 3600, # decimal hour of the day
         week = (tn %% (3600 * 24 * 7)) / 24 / 3600, # decimal day of the week
         stop(paste("unknown value for by: ", by)))
}

if(!isGeneric("pv_stcube"))
  setGeneric("pv_stcube", function(x, ...)
    standardGeneric("pv_stcube"))

pv_stcube.sfTrack <- function(x, value, map=FALSE, ...){
  coords = sf::st_coordinates(x@geometry)
  xlim = c(st_bbox(x@geometry)[[1]], st_bbox(x@geometry)[[3]])
  ylim = c(st_bbox(x@geometry)[[2]], st_bbox(x@geometry)[[4]])
  time = index(x@time)
  time <- time - min(time)
  if(missing(value)){
    plot3Drgl::scatter3Drgl(x = coords[, 1], y = coords[, 2], z = time, xlab = "x", ylab = "y", zlab = "t",
                            ticktype = "detailed",
                            clab = "Time")

  }

  else{
    plot3Drgl::scatter3Drgl(x = st_coordinates(x@geometry)[,1], y = st_coordinates(x@geometry)[,2], z = time,
                            xlim = xlim,
                            ylim = ylim,
                            colvar = x@data[[value]],
                            ticktype = "detailed",
                            clab = value,
                            xlab = "x", ylab = "y", zlab ="t")
  }
  if(map){
    maplimx = xlim + c(-0.1,0.1) * diff(xlim)
    maplimy = ylim + c(-0.1,0.1) * diff(ylim)
    map <- OSM(xlim = maplimx, ylim= maplimy, mapType = "osm", mapZoom = NULL, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
    map3d(map, z = time[1], add=T)
  }
}

setMethod("pv_stcube", "sfTrack", pv_stcube.sfTrack)

pv_stcube.sfTracks <- function(x, value, map=FALSE, normalizeBy = "week", xlab='x', ylab='y', zlab='z', ...){

  xlim = c(st_bbox(x@tracks[[1]]@geometry)[[1]], st_bbox(st_bbox(x@tracks[[1]]@geometry))[[3]])
  ylim = c(st_bbox(x@tracks[[1]]@geometry)[[2]], st_bbox(x@tracks[[1]]@geometry)[[4]])

  dim = length(x@tracks[[1]]@geometry)
  coordsAll = sft_coordinates(x)
  timeAll = normalize(do.call(c, lapply(x@tracks,
                                        function(x) index(x@time))), normalizeBy)
  zlim = range(timeAll)
  col = rainbow(length(x@tracks))

  if(missing(value)){
    rgl::plot3d(x = coordsAll[1:dim, 1], y = coordsAll[1:dim, 2],
                z = timeAll[1:dim], col=col[1], xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, zlab=zlab)

    tracks = x@tracks[-1]
    for(t in seq_along(tracks)) {
      coords = sf::st_coordinates(tracks[[t]]@geometry)
      time = normalize(index(tracks[[t]]@time), normalizeBy)
      rgl::lines3d(x = coords[, 1], y = coords[, 2], z = time, col = col[t+1])
    }
  }
  else{
    rgl::plot3d(x = coordsAll[1:dim, 1], y = coordsAll[1:dim, 2],
                z = timeAll[1:dim], col=col[1], xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, zlab=zlab)

    tracks = x@tracks
    for(t in seq_along(tracks)) {
      coords = sf::st_coordinates(tracks[[t]]@geometry)
      time = normalize(index(tracks[[t]]@time), normalizeBy)
      plot3Drgl::scatter3Drgl(x = coords[, 1], y = coords[, 2], z = time, colvar = tracks[[t]]@data[[value]], add=TRUE)
    }
  }

  if(map){
    maplimx = xlim + c(-0.1,0.1) * diff(xlim)
    maplimy = ylim + c(-0.1,0.1) * diff(ylim)
    map <- OSM(xlim = maplimx, ylim= maplimy, mapType = "osm", mapZoom = NULL, projection = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84")
    map3d(map, z = timeAll[1], add=T)
  }
}

setMethod("pv_stcube", "sfTracks", pv_stcube.sfTracks)


if(!isGeneric("intersection_cube"))
  setGeneric("intersection_cube", function(x, ...)
    standardGeneric("intersection_cube"))




#TODO:
# Add intersection points
intersection_cube.sfTracks <- function(x, map=FALSE, normalizeBy = "week", xlab='x', ylab='y', zlab='z', ...){
  xlim = c(st_bbox(x@tracks[[1]]@geometry)[[1]], st_bbox(st_bbox(x@tracks[[1]]@geometry))[[3]])
  ylim = c(st_bbox(x@tracks[[1]]@geometry)[[2]], st_bbox(x@tracks[[1]]@geometry)[[4]])

  dim = length(x@tracks[[1]]@geometry)
  coordsAll = sft_coordinates(x)
  timeAll = normalize(do.call(c, lapply(x@tracks,
                                        function(x) index(x@time))), normalizeBy)
  zlim = range(timeAll)
  col = rainbow(length(x@tracks))

  i_points = sf::st_intersection(sft63@line, sft64@line)

  rgl::plot3d(x = coordsAll[1:dim, 1], y = coordsAll[1:dim, 2],
                z = timeAll[1:dim], col=col[1], xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, zlab=zlab)
  tracks = x@tracks[-1]
  for(t in seq_along(tracks)) {
    coords = sf::st_coordinates(tracks[[t]]@geometry)
    time = normalize(index(tracks[[t]]@time), normalizeBy)
    rgl::lines3d(x = coords[, 1], y = coords[, 2], z = time, col = col[t+1])
  }
}

setMethod("intersection_cube", "sfTracks", intersection_cube.sfTracks)
