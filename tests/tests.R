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

library(stdbscanr)
library(raster)
tcagg_xy <- sfc_as_cols(trackcol_agg)
tcagg_xy <- data.frame(tcagg_xy, timestamp = 1:nrow(tcagg_xy))
c = get_clusters_from_data(tcagg_xy
                       ,x = "x"
                       ,y = "y"
                       ,t = 'timestamp'
                       ,eps = 2 # 2 metre distance threshold from other point
                       ,eps_t = 10 # 10 second time threshold from other point
                       ,minpts = 10)



### MODIFICATION OF
### https://github.com/gdmcdonald/stdbscanr
### Author: Gordon McDonald
find_temporally_connected_points<-function(vec, eps_t){

  sparse_sim <- vector("list", length(vec))

  for (i in 1:length(vec)){

    #count forwards
    fcounter <- 0
    while (T){
      if (length(vec) < i + fcounter + 1) break
      if (abs(vec[i + fcounter + 1] - vec[i]) <= eps_t) break
      fcounter <- fcounter + 1
    }


    #count backwards
    bcounter <- 0
    while (T){
      if (0 >= i - bcounter - 1 ) break
      if (abs(vec[i] - vec[i - bcounter - 1])  >= eps_t) break
      bcounter <- bcounter + 1
    }


    #write indicies of elements within range as a list element
    sparse_sim[[i]] <- (i-bcounter):(i+fcounter)



  }

  #collapse the sparse simmilarity list into a data.table
  names(sparse_sim) <- 1:length(vec)
  sparse_sim <- utils::stack(sparse_sim)
  data.table::setDT(sparse_sim)

  #return just the columns we need
  return(
    sparse_sim[,
               ind:=as.integer(as.character(ind))
    ][,.(first = ind, second = values)]
  )
}



#' Find spatially close points
#'
#' Find connected points less than `eps` away from each other in space,
#' given a list of those points which are sufficiently connected in time.
#'
#' @param dt data.table of points with columns `id`, `x`, `y`.
#' @param sls The sparse logical similarity matrix output from `find_temporally_connected_points()`.
#' @param eps The max distance between two points for them to be "connected".
#' @return A data.table with two columns, "first" and "second". These are the indicies of
#' the connected points, and the points it is connected to.
#' @examples
#' dt <- data.table(id = 1:100,
#'                 x=rnorm(100),
#'                 y = rnorm(100),
#'                 t = 1:100)
#' find_spatially_connected_points(dt = dt,
#'                       sls = find_temporally_connected_points(vec = dt$t,
#'                                              eps_t = 1.5),
#'                       eps = 10)
find_spatially_connected_points <- function(dt, sls, eps){

  #merge with coordinates of first point, second point
  int<-data.table::merge.data.table(sls, dt, by.x = "first", by.y = "id",suffixes = c("",".first"),all.x = T)
  fin<-data.table::merge.data.table(int, dt, by.x = "second", by.y = "id",suffixes = c("",".second"),all.x = T)

  #compute distance and check if under threshold
  fin[,dist:=sqrt((x-x.second)^2+(y-y.second)^2)][,close:=dist<=eps]
  #output only "reachable" points
  out<- fin[close==T,.(first,second)]
  return(out)
}

#' Find connected points less than `eps` away from each other in space,
#' and less than `eps_t` away from each other in time, given a data.table of those points
#' given a list of those points which are sufficiently connected in time.
#'
#' @param dt data.table of points with columns `id`, `x`, `y`, `time`.
#' @param eps The max distance between two points for them to be "connected".
#' @param eps_t The max time between two points for them to be "connected".
#' @return A data.table with two columns, "first" and "second". These are the indicies of
#' the connected points, and the points it is connected to.
#' @examples
#' dt <- data.table(id = 1:100,
#'                 x=rnorm(100),
#'                 y = rnorm(100),
#'                 t = 1:100)
#' find_connected_points(dt = dt,
#'                sls = find_temporally_connected_points(vec = dt$t,
#'                                       eps_t = 1.5),
#'                eps = 10)
find_connected_points <- function(dt, eps, eps_t){

  sls <- find_temporally_connected_points(vec = dt$time,
                                          eps_t = eps_t)

  reachables <- find_spatially_connected_points(dt = dt,
                                                sls = sls,
                                                eps = eps)

  return(reachables)
}


#' Remove noise
#'
#' Remove connections from a 'first' point if that point doesn't have enough points
#' around it (`min_points`) that it is connected to.
#'
#' @param reachables data.table of connections with columns `first`, `second` which
#' has been output from `find_connected_points()`
#' @param min_number The minimum number of connected `second` points for this `first` point to be kept.
#' @return A data.table with two columns, `first` and `second`. These are the indicies of
#' the connected points, and the points it is connected to.
#' @examples
#' dt <- data.table(id = 1:100,
#'                 x=rnorm(100),
#'                 y = rnorm(100),
#'                 t = 1:100)
#' reachables <- find_connected_points(dt = dt,
#'                sls = find_temporally_connected_points(vec = dt$t,
#'                                       eps_t = 1.5),
#'                eps = 10)
#' remove_noise(reachables = reachables, min_number = 4)
remove_noise <- function(reachables, min_number = 4){
  #remove all firsts which don't have min_number seconds
  noise_free_reachables <- reachables[,if(.N>=min_number) .SD,by = first]

  if(nrow(noise_free_reachables)==0){
    stop("Error: All points removed as noise. Try setting min points to be smaller or eps and eps_t to be biggger.")
  }

  return(noise_free_reachables)
}

#' One cluster iteration
#'
#' One iteration of finding connected groups of points and labelling them as the same cluster.
#' Takes a data.table of clusters and merges it with itself to find the next layer of connections,
#' and relables clusters with the minimum of their two previous cluster numbers.
#'
#' @param clusters data.table of connections with columns `first`, `second` and `cluster`
#' @return A data.table of connections with columns `first`, `second` and `cluster`,  in which the cluster numbers have been updated.
cluster_iteration <- function(clusters){

  # One row per t=(n-1) seed-cluster connection
  cluster2<-unique(clusters[,.(first,cluster)])

  # Merge with itself and take minimum cluster number
  # to get t=n seed-cluster connection
  clusters_second <- data.table::merge.data.table(clusters,
                                                  cluster2,
                                                  by.x = "second",
                                                  by.y = "first",
                                                  suffixes = c("1","2"),
                                                  all.x = T)

  clusters_third <- clusters_second[,cluster:=min(c(cluster1,
                                                    cluster2),
                                                  na.rm = T),by=first]

  return(clusters_third[,.(first, second, cluster)])
}

#' Find final equilibrium clusters
#'
#' Iterates through expanding clusters until finished, then returns the final clusters.
#'
#' @param clusters data.table of connections with columns `first`, `second` and `cluster`
#' @return A data.table of connections with columns `first`, `second` and `cluster`,  in which the cluster numbers are final.
find_equilibrium_clusters <- function(clusters){

  iter_count <- 0
  while (T){
    #some debugging output - this can be removed later.
    iter_count <- iter_count + 1

    #store a copy of before the iteration to compare to afterwards
    clusters_old <- clusters

    #iterate giving the cluster labels of firsts to their seconds
    clusters <- cluster_iteration(clusters = clusters_old)

    #if nothing changed in an iteration, we are done!
    if (T==all.equal(clusters_old, clusters)) break
  }
  message(iter_count," iterations to equilibrium cluster configuration")

  return(clusters)
}


#' Get Clusters from Data
#'
#' Takes a dataframe/tibble/data.table with identified columns for x, y, t and finds
#' clusters of points that are connected through being no more than `eps` apart in space,
#' no more than `eps_t` apart in time, and connected through points with a minimum of `min_number` connections.
#'
#' @param df dataframe/data.table/tibble containing trajectories to cluster.
#' @param x string, name of the x spatial column
#' @param y string, name of the y spatial column
#' @param t string, name of the timestamp column
#' @param eps Largest distance apart points can be to be directly connected
#' @param eps_t Longest time apart points can be to be directly connected
#' @param minpts Smallest number of points a point must be connected to, to
#' not be excluded as a possible source point for connecting further points to a cluster.
#' @return A data.table that looks like `df` but contains two additional columns, `point_density` and `cluster`.
#' @export
#' @examples
#' dt <- data.table(id = 1:100,
#'                 X = rnorm(100),
#'                 Y = rnorm(100),
#'                 timestamp = 1:100)
#' get_clusters_from_data(dt
#'                        ,x = "X"
#'                        ,y = "Y"
#'                        ,t = "timestamp"
#'                        ,eps = 2 # 2 metre distance threshold from other point
#'                        ,eps_t = 10 # 10 second time threshold from other point
#'                        ,minpts = 3) # minimum connected to 3 points to continue growing a cluster
get_clusters_from_data <- function(df
                                   ,x = "X"
                                   ,y = "Y"
                                   ,t = "timestamp"
                                   ,eps = 1 # metre from other point
                                   ,eps_t = 5 # seconds from other point
                                   ,minpts = 7){

  # set up input data - set it to be data.table
  data.table::setDT(df)

  #sort by time
  data.table::setkeyv(df,cols = t)

  #create id column
  df[,id:=.I]

  # get the right columns
  dt = df[,c("id", x, y, t),with = F]

  #name them how the other functions expect
  data.table::setnames(dt,
                       old = c("id",   x,   y,  t),
                       new = c("id", "x", "y", "time"))


  # find reachables
  reachables <- find_connected_points(dt = dt,
                                      eps = eps,
                                      eps_t = eps_t)


  point_density <- reachables[,.(density = .N),by = first]

  data.table::setnames(point_density,old = c("first"),new = c("id"))


  # remove noise (clusters with less than minpts)
  reachables_without_noise <- remove_noise(reachables = reachables,
                                           min_number = minpts)

  # initially set cluster_id to first index
  clusters <- reachables_without_noise[,cluster:=first]

  #sort by first for faster merge
  setkey(clusters,first)

  #iterate until final clusters are here
  clusters <- find_equilibrium_clusters(clusters = clusters)

  #rename the clusters to be sequential starting from 1
  cluster_numbers_old <- na.omit(unique(clusters$cluster))
  cluster_numbers_new <- 1:length(cluster_numbers_old)
  names(cluster_numbers_new) <- cluster_numbers_old

  #extract just the point ids and the cluster they belong to
  point_to_clust <- unique(clusters[,.(point_id = second,
                                       cluster = cluster_numbers_new[as.character(cluster)])])

  temp <- data.table::merge.data.table(x = df,
                                       y = point_to_clust[,.(point_id,
                                                             cluster)],
                                       by.x = "id",
                                       by.y = "point_id",
                                       all.x = T)

  df_out <- data.table::merge.data.table(x = temp,
                                         y = point_density,
                                         by.x = "id",
                                         by.y = "id",
                                         all.x = T)

  return(df_out[,id:=NULL])
}


#' Create demo data to test clustering algorithm
#'
#' @param n number of rows in demo data
#'
#' @return A new data.table with `n` rows to feed into the clustering algorithm
#' @export
#'
#' @examples
#' demo_data(n = 1000)
demo_data <- function(n = 1000){
  dt = data.table(timestamp = 1:n,
                  X = rnorm(n),
                  Y = rnorm(n))
}
