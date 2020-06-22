plot.sfTrack <- function(x, y, ...){
  if(missing(y)){
    ggplot() +
      geom_sf(data = x@line)
  }
  else{
    ggplot() +
      geom_sf(data = x@geometry, aes(color = x@data[[y]])) +
      scale_color_viridis_c() +
      labs(color = y)
  }
}

setMethod("plot", "sfTrack", plot.sfTrack)

plot(sft1)
plot(sft1, "CO2.value")
plot(sft1, "Speed.value")

plot.sfTracks <- function(x, y, ...){
  if(missing(y)){
    p <- ggplot()
    for(i in 1:length(x@tracks)){
      p <- p + geom_sf(data = x@tracks[[i]]@line, aes(color = x@tracks[[!!i]]@id[[1]]))
    }
    p + labs(color = "Track ID")
  }
  else{
    p <- ggplot()
    for(i in 1:length(x@tracks)){
      p <- p + geom_sf(data = x@tracks[[i]]@geometry, aes(color = x@tracks[[!!i]]@data[[y]]))
    }
    p + scale_color_viridis_c() +
      labs(color = y)
  }
}

setMethod("plot", "sfTracks", plot.sfTracks)
plot(sftc)

