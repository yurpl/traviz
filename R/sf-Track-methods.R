st_transform.sfTrack <- function(x, crs = ""){
  x@line = sf::st_transform(x@line, crs)
  x@geometry = sf::st_transform(x@geometry, crs)
  return(x)
}

setMethod("st_transform", "sfTrack", st_transform.sfTrack)

sft1 <- st_transform(sft1, "+proj=utm")
sft2 <- st_transform(sft2, "+proj=utm")


st_distance.sfTrack <- function(track1, track2, which = ""){
  dist <- st_distance(track1@line, track2@line, which = which)
  return(dist)
}

