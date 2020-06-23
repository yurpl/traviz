transform.sfTrack <- function(x, crs = ""){
  x@line = sf::st_transform(x@line, crs)
  x@geometry = sf::st_transform(x@geometry, crs)
  return(x)
}

setMethod("transform.sfTrack", "sfTrack", transform.sfTrack)

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
      function(from) as(from@data, "data.frame")
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

