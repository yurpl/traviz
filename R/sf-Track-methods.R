st_transform.sfTrack <- function(x, crs = ""){
  x@line = sf::st_transform(x@line, crs)
  x@geometry = sf::st_transform(x@geometry, crs)
  return(x)
}

setMethod("st_transform", "sfTrack", st_transform.sfTrack)

sft1 <- st_transform(sft1, "+proj=utm")
sft2 <- st_transform(sft2, "+proj=utm")

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


setGeneric(
  name = "distance",
  def = function(sft1, sft2, ...) standardGeneric("distance")
)

distance.sfTrack <- function(sft1, sft2, which = ""){
  dist <- sf::st_distance(sft1@line, sft2@line, which = which)
  return(dist)
}

setMethod("distance", "sfTrack", distance.sfTrack)

distance(sft1, sft2, "Frechet")

print.sfTrack <- function(sft){
  track = sft
  cat("An object of class sfTrack \n");
  cat(paste0(nrow(as.data.frame(track@geometry)), "points"),"\n");
  cat(paste0("bbox:"),"\n");
  print(sf::st_bbox(track@geometry));
  cat(paste0("Time period: [",min(track@time),", ", max(track@time),"]"))
}
setMethod("show", "sfTrack", print.sfTrack)

print.sfTracks <- function(sft){
  cat("An object of class sfTracks" ,"\n");
  cat(paste0(length(sft@tracks)), "sftracks")
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

#sf methods for ease of use when working with sfTracks
setMethod("st_bbox", "sfTrack",
          function(obj) {
            st_bbox(obj@line)
          })

setMethod("stcube", "sfTrack",
          function(x, ...){
            trajectories::stcube(as(x, "Track"))
            })

setMethod("st_length", "sfTrack",
          function(x){
            sf::st_length(x@line)
          })

