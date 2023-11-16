library(GeoLocTools)
library(lubridate)
library(SGAT)
library(TwGeos)
library(tidyverse)

move_1947021 <- read_csv("data/gls/1947021.csv")
lightraw <- move_1947021 %>%
  select(Date = timestamp,
         Light = `gls:light-level`) %>%
  slice(seq(1, nrow(.), by = 5))
lon.calib <- -64.1
lat.calib <- -64.8

threshold <- 1
offset <- 18 # adjusts the y-axis to put night (dark shades) in the middle
# (0 to 4, or 23 depending on files - no negative values!)
lightImage( tagdata = lightraw,
            offset = offset,
            zlim = c(0, 20))
tsimageDeploymentLines(lightraw$Date, lon = lon.calib, lat = lat.calib,
                       offset = offset, lwd = 3,
                       col = adjustcolor("orange", alpha.f = 0.5))
twl <- preprocessLight(lightraw,
                       threshold = threshold,
                       offset = offset,
                       lmax = 11,         # max. light valu
                       gr.Device = "x11")
saveRDS(twl, "data/gls/1947021twl.rds")
twl <- readRDS("data/gls/1947021twl.rds")

# View twilights
lightImage(tagdata = lightraw,
           offset = offset,
           zlim = c(0, 20))
tsimagePoints(twl$Twilight, offset = offset, pch = 16, cex = 1.2,
              col = ifelse(twl$Rise, "firebrick", "cornflowerblue"))


# initial path
initialPath <- function (twilight, rise, time = twilight, zenith = 96, tol = 0.08, unfold = TRUE)
{
  ls <- thresholdLocation(twilight, rise, zenith = zenith, tol = tol)
  if (!is.null(time)) {
    keep <- !is.na(ls$x[, 1L])
    ts <- ls$time[keep]
    lon <- ls$x[keep, 1L]
    if (unfold) lon <- unwrapLon(lon)
    lon <- approx(x = ts, y = lon, xout = time, rule = 2)$y
    eq  <- is.na(ls$x[, 2L])
    keep <- !is.na(ls$x[, 2L])
    ts <- ls$time[keep]
    lat <- ls$x[keep, 2L]
    lat <- approx(x = ts, y = lat, xout = time, rule = 2)$y
    lat[eq] <- NA
    ls <- list(time = time, x = cbind(lon, lat))
  }
  ls
}

z <- zenith(solar(twl$Twilight[1]), lon.calib, lat.calib)
init <- initialPath(twl$Twilight, twl$Rise, zenith = z)
init_df <- data.frame(
  lon = init$x[,1],
  lat = approx(init$time[!is.na(init$x[, 2])],
               init$x[!is.na(init$x[,2]), 2],
               xout = init$time)$y
)
library(sf)
library(tmap)
init_pts <- st_as_sf(init_df, coords = c("lon", "lat"), crs = "epsg:4326")
init_line <- tibble(geometry = list(st_linestring(as.matrix(init_df)))) %>%
  st_as_sf(crs = "epsg:4326")

map_crs <- st_crs('+proj=laea +lon_0=-70.4882812 +lat_0=-60.5044497 +datum=WGS84 +units=m +no_defs')
map_bbox <- st_bbox(c(xmin = -100, ymin = -88, xmax = -60, ymax = -40),
                    crs = "epsg:4326") %>%
  st_as_sfc() %>%
  st_transform(map_crs) %>%
  st_bbox()

tm_shape(terra::rast("data/basemap/natgeobasemap_modified.tif")) +
  tm_rgb() +
  tm_shape(init_line, projection = map_crs, bbox = map_bbox) +
  tm_lines() +
  tm_shape(init_pts) +
  tm_symbols(col = "firebrick", size = 0.1)
