#Singular sfTrack
setClass("sfTrack",
         representation(
           id = "character",
           data = "data.frame",
           time = "POSIXct",
           geometry = "sfc",
           line = "sfc"
         ))

sfTrack = function(df, identifier){
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
  df.traj.un <- unnest(df.traj)
  new("sfTrack", id = df.traj$track.id, data = data.frame(df.traj$data), time = df.traj.un$time, geometry = df.traj.un$geometry, line = df.traj$geometry)
}

trajectories <- ec[1:500,]
trajectories2 <- ec[600:1000,]
sft1 = sfTrack(trajectories, "track.id")
sft2 = sfTrack(trajectories2, "track.id")

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
