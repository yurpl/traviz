library(raster)
library(RColorBrewer)
library(lubridate)



t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)

ec$time <- gsub("T", " ", ec$time)
ec$time <- as.POSIXct(ec$time)
ec <- st_as_sf(ec, wkt = "geometry")


trackcol <- ec.trj

test_reg <- aggregate_sf_roi(trackcol, 7.6, 7.8, 51.8, 52.5)

i <- st_intersection(test_reg)





trackcol_un <- unnest(trackcol)



trackcol_agg <- trackcol_un %>%
  filter(!is.na(Speed.value)) %>%
  #mutate(time2 = format(time, format="%H:%M:%S")) %>%
  filter(time >= "2019-12-24 15:25:33" & time <= "2019-12-25 15:25:33")
  #select(-time2)

trackcol_agg <- aggregate_sf_roi(trackcol_un, 7.56, 7.7, 51.92, 52)
trackcol_agg <- trackcol_agg %>%
  filter(!is.na(Speed.value))



rast <- raster(trackcol_agg, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = .0005)

#cropped <- crop(rast, extent(c(7.584686,7.677040,51.947122,51.979171)))
#crs(cropped) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"

#rs <- resample(rast, cropped)

r<-rasterize(trackcol_agg, rast, trackcol_agg$Speed.value)


crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"


gs <- gstat(formula=Speed.value~1, data = trackcol_agg)
i=interpolate(rast, gs)

interpolated = mask(i,r)
crs(interpolated) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"


tm_shape(r) + tm_raster(style= "cont", palette = cols)




library(RColorBrewer)
cols <- brewer.pal(5, "RdYlGn")



mapview::mapview(hex_points, col.region = cols)



ggplot() + geom_stars(data = st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_, pretty=TRUE))
) + scale_fill_gradientn(colors = cols) + ylim(c(51.5, 52.5)) + xlim(7.5,7.8)

st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_))


hex_points = data.frame(rasterToPoints(r))
hex_points<-st_as_sf(hex_points)
st_intersection(hex_points, t)
ggplot(hex_points) + geom_sf(data = hex_points, aes(fill = layer)) +   scale_fill_viridis_c(option = "magma")
grid <- st_make_grid(hex_points)
ggplot(hex_points) + geom_sf(data = st_union(hex_points)) + geom_sf(data =grid, fill = NA) + scale_fill_viridis_c(option = "magma")



library(gganimate)
p <- ggplot(t) + geom_sf()
anim <- p + transition_states(time) + shadow_wake(0.5, alpha = FALSE)
animate(anim, duration = 10, fps = 20, width =600, height = 600, renderer = gifski_renderer())


pspat <- as.ppp(t)


spi <- as_Spatial(i_points$geometry)
spi <- spTransform(spi, CRS("+proj=longlat +datum=WGS84"))
