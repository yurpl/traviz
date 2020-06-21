#Singular sfTrack
setClass("sfTrack",
         representation(
           id = "character",
           data = "data.frame",
           time = "POSIXct",
           geometry = "sfc",
           line = "sfc"
         ))

sfTrack = function(df){
  if(!is(df, "sf") || !is(df, "data.frame")){
    df <- geodata_to_sf(df)
  }
  df.un <- df %>% unnest
  df.un$geometry <- df.un$geometry %>% st_set_crs(4326)
  new("sfTrack", id = df$track.id, data = data.frame(df$data), time = df.un$time, geometry = df.un$geometry, line = df$geometry)
}

trajectories1 <- ec.trj[55,]
trajectories2 <- ec.trj[56,]
sft1 = sfTrack(trajectories1)
sft2 = sfTrack(trajectories2)

#Multiple sftracks
setClass("sfTracks",
         representation(tracks = "list")
         )

sfTracks = function(tracks){
  stopifnot(is.list(tracks))
  names(tracks) = paste("Track", 1:length(tracks), sep = "")
  new("sfTracks", tracks = tracks)
}
sftc <- sfTracks(c(sft1, sft2))


