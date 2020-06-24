# # #FILE FOR TESTING SCRIPTS AND IDEAS
# #
# #
# library(raster)
# library(RColorBrewer)
# library(lubridate)
# library(trajectories)
# library(tidyverse)
# library(sp)
# library(sf)
# library(spacetime)
# #
#
# ec <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
# #
#
# t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
# t$time <- gsub("T", " ", t$time)
# trajectories <- traviz::geodata_to_sf(t, "track.id")
#
# t <- t[1:10,]
#
#
# test_track1 <- as.sf.Tracks(t[6,])
# test_track2 <- as.sf.Tracks(t[4,])
# ec$time <- gsub("T", " ", ec$time)
# ec$time <- as.POSIXct(ec$time)
# ec$time <- ymd_hms(ec$time)
#
# ec <- st_as_sf(ec, wkt = "geometry")
# # #
# # # to_line <- function(tr) st_cast(st_combine(tr), "LINESTRING") %>% .[[1]]
# # #
# # # ec.nest <- ec %>% group_by(track.id) %>% nest
# # #
# # #
# # # tracks <- ec.nest %>% pull(data) %>% map(to_line) %>% st_sfc(crs = 4326)
# # #
# # # ec.trj <- ec.nest %>% st_sf(geometry = tracks)
# # #
# # # #Choose one track to make testing easier
# tracktest <- ec.trj[1,]
# tracktest <- tracktest %>% unnest
# # #
# # spdf <- as_Spatial(track1)
# #
# # space<-STIDF(geometry(spdf), spdf$time, spdf@data)
# # trajformatted <- Track(space)
# #
# #
# #
# #
# #
# #
# # t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)
# #
# # ec$time <- gsub("T", " ", ec$time)
# # ec$time <- as.POSIXct(ec$time)
# # ec <- st_as_sf(ec, wkt = "geometry")
# #
# #
# # trackcol <- ec.trj
# #
# # test_reg <- aggregate_sf_roi(trackcol, 7.6, 7.8, 51.8, 52.5)
# #
# # i <- st_intersection(test_reg)
# #
# #
# #
# #
# #
# # trackcol_un <- unnest(trackcol)
# #
# #
# #
# # trackcol_agg <- trackcol_un %>%
# #   filter(!is.na(Speed.value)) %>%
# #   #mutate(time2 = format(time, format="%H:%M:%S")) %>%
# #   filter(time >= "2019-12-24 15:25:33" & time <= "2019-12-25 15:25:33")
# #   #select(-time2)
# #
# # trackcol_agg <- aggregate_sf_roi(trackcol_un, 7.56, 7.7, 51.92, 52)
# # trackcol_agg <- trackcol_agg %>%
# #   filter(!is.na(Speed.value))
# #
# #
# #
# # rast <- raster(trackcol_agg, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = .0005)
# #
# # #cropped <- crop(rast, extent(c(7.584686,7.677040,51.947122,51.979171)))
# # #crs(cropped) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
# #
# # #rs <- resample(rast, cropped)
# #
# # r<-rasterize(trackcol_agg, rast, trackcol_agg$Speed.value)
# #
# #
# # crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
# #
# #
# # gs <- gstat(formula=Speed.value~1, data = trackcol_agg)
# # i=interpolate(rast, gs)
# #
# # interpolated = mask(i,r)
# # crs(interpolated) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
# #
# # library(tmap)
# # tm_shape(rasterize(i_points, raster(i_points, res=.0005), i_points$n.overlaps)) + tm_raster(style= "cont", palette = cols)
# #
# #
# #
# #
# # library(RColorBrewer)
# # cols <- brewer.pal(5, "RdYlGn")
# #
# #
# #
# # mapview::mapview(hex_points, col.region = cols)
# #
# #
# #
# # ggplot() + geom_stars(data = st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_, pretty=TRUE))
# # ) + scale_fill_gradientn(colors = cols) + ylim(c(51.5, 52.5)) + xlim(7.5,7.8)
# #
# # st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_))
# #
# #
# # hex_points = data.frame(rasterToPoints(r))
# # hex_points<-st_as_sf(hex_points)
# # st_intersection(hex_points, t)
# # ggplot(hex_points) + geom_sf(data = hex_points, aes(fill = layer)) +   scale_fill_viridis_c(option = "magma")
# # grid <- st_make_grid(hex_points)
# # ggplot(hex_points) + geom_sf(data = st_union(hex_points)) + geom_sf(data =grid, fill = NA) + scale_fill_viridis_c(option = "magma")
# #
# #
# #
# # library(gganimate)
# # p <- ggmap(get_map(location = 'Europe', zoom = 4)) + geom_sf(data=t, aes(fill=track.id))
# # anim <- p + transition_states(time) + shadow_wake(0.5, alpha = FALSE)
# # animate(anim, duration = 10, fps = 20, width =600, height = 600, renderer = gifski_renderer())
# #
# #
# # pspat <- as.ppp(t)
# #
# #
# # spi <- as_Spatial(i_points$geometry)
# # spi <- spTransform(spi, CRS("+proj=longlat +datum=WGS84"))
# #
# # plot(density(as.ppp(spi@coords, as.owin(hexes))))
# #
# # stdbscan(trackcol_agg$geometry, trackcol_agg$time, 1, 7, 10)
# #
# #
# # track1 <- st_transform(track1, crs = "+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
# #
# # cl <- hclust(as.dist(st_distance(track1)))
# # track1$class5 = as.factor(cutree(cl, 5))
# # plot(track1[,"class5"])
# # track1$class5 = as.factor(cutree(cl, 3))
# #
# #
# #
# lines <- as_Spatial(track1$geometry1)
# lines <- spTransform(lines, CRS("+proj=utm +zone=15 +ellps=WGS84"))
#
# linepsp <-  maptools::as.psp.SpatialLines(lines, owin())
# plot(density(linepsp))
# #
# #
# # density(as.ppp(spi@coords, as.owin(spi)))
# # plot(density(as.ppp(spi@coords, as.owin(st_bbox(hex_points)))))
# #
# #
# # test_rast <- sf_to_raster(trackcol_agg, "Speed.value", .0009)
# # rast_points <- data.frame(rasterToPoints(test_rast))
# # rast_points <- st_as_sf(rast_points, coords=c("x","y"), crs="+proj=utm +zone=15 +ellps=WGS84 +units=m +no_defs")
# # rast_points <- as_Spatial(rast_points)
# # rast_points <- maptools::as.ppp.SpatialPointsDataFrame(rast_points)
# #
# # contour(density(rast_points, at="pixels", we
#
# ights = rast_points$marks))
# # plot(rast_points, add=TRUE)
#
# sfc_as_cols <- function(x, names = c("x","y")) {
#   stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
#   ret <- sf::st_coordinates(x)
#   ret <- tibble::as_tibble(ret)
#   stopifnot(length(names) == ncol(ret))
#   x <- x[ , !names(x) %in% names]
#   ret <- setNames(ret,names)
#   dplyr::bind_cols(x,ret)
# }
#
# track1 <- sfc_as_cols(track1)
# library(moveVis)
# move <- df2move(track1, proj ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", x='x', y='y', time='time' )
# m <- align_move(move, res = 5, unit = "mins")
# frames <- frames_spatial(m, path_colours = c("red"), map_service = "osm", map_type = "watercolor", alpha = 0.5) %>%
#   add_labels(x = "Longitude", y = "Latitude") %>%
#   add_northarrow() %>%
#   add_scalebar() %>%
#   add_timestamps(m, type = "label") %>%
#   add_progress()
#
# animate_frames(frames, out_file = "trajectory.gif")

#Append data frame to sfTracks (TODO: MAKE A METHOD)

# list_nest <- list()
# for(i in 1:length(test_nest)){
#   track <- traviz::sfTrack(test_nest[i,])
#   list_nest <- append(list_nest, track)
# }
# sfTracks(list_nest)

