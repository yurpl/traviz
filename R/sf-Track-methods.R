st_transform.sfTrack <- function(x, crs = ""){
  x@line = sf::st_transform(x@line, crs)
  x@geometry = sf::st_transform(x@geometry, crs)
  return(x)
}

setMethod("st_transform", "sfTrack", st_transform.sfTrack)

sft1 <- st_transform(sft1, "+proj=utm")
sft2 <- st_transform(sft2, "+proj=utm")

setGeneric(
  name = "st_distance",
  def = function(sft1, sft2, ...) standardGeneric("st_distance")
)


st_distance.sfTrack <- function(sft1, sft2, which = ""){
  dist <- sf::st_distance(sft1@line, sft2@line, which = which)
  return(dist)
}

setMethod("st_distance", "sfTrack", st_distance.sfTrack)

st_distance(sft1, sft2, "Frechet")
