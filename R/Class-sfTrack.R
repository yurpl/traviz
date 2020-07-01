#' A Track in sf format
#'
#' @slot id unique id value for track
#' @slot data dataframe of track data
#' @slot time POSIXct format timestamps
#' @slot geometry geometry of track in sfc format
#' @slot line LINESTRING trajectory of track
#'
setClass("sfTrack",
         representation(
           id = "character",
           data = "data.frame",
           time = "POSIXct",
           geometry = "sfc",
           line = "sfc"
         ))

sfTrack = function(df, identifier){
  if(!is(df, "sf")){
    df <- geodata_to_sf(df, identifier)
  }
  df.un <- df %>% unnest
  df.un$geometry <- df.un$geometry %>% st_set_crs(4326)
  new("sfTrack", id = df$track.id, data = data.frame(df$data), time = df.un$time, geometry = df.un$geometry, line = df$geometry)
}

#' Multiple sfTracks
#' @slot list list of sfTracks
setClass("sfTracks",
         representation(tracks = "list")
         )

sfTracks = function(tracks){
  stopifnot(is.list(tracks))
  names(tracks) = paste("Track", 1:length(tracks), sep = "")
  new("sfTracks", tracks = tracks)
}


