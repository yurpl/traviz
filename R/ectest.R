library(raster)
library(RColorBrewer)
library(lubridate)



t <- read.csv("tracks.csv",header = TRUE, check.names = TRUE)

ec$time <- gsub("T", " ", ec$time)
ec$time <- ymd_hms(ec$time)
ec <- st_as_sf(ec, wkt = "geometry")


trackcol <- ec.trj
trackcol_un <- unnest(trackcol)



trackcol_agg <- trackcol_un %>%
  filter(!is.na(Speed.value)) %>%
  mutate(time2 = format(time, format="%H:%M:%S")) %>%
  filter(time2 >= "16:00:00" & time2 <= "18:00:00") %>%
  select(-time2)




rast <- raster(trackcol_agg, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", res = .01)

cropped <- crop(rast, extent(c(7.584686,7.677040,51.947122,51.979171)))
crs(cropped) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"

#rs <- resample(rast, cropped)

r<-rasterize(trackcol_agg, cropped, trackcol_agg$Speed.value)


crs(r) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"


tm_shape(r) + tm_raster(style= "cont", palette = cols)




library(RColorBrewer)
cols <- brewer.pal(5, "RdYlGn")



mapview::mapview(r, col.region = cols)



ggplot() + geom_stars(data = st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_, pretty=TRUE))
) + scale_fill_gradientn(colors = cols) + ylim(c(51.5, 52.5)) + xlim(7.5,7.8)

st_rasterize(trackcol_agg[,24], st_as_stars(st_bbox(trackcol_agg), values = NA_real_))







