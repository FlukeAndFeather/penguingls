library(lubridate)
library(readxl)
library(sf)
library(SGAT)
library(tidyverse)
library(tmap)
library(TwGeos)

# PARAM
id <- "1947026"
threshold <- 1
offset <- 18 # adjusts the y-axis to put night (dark shades) in the middle

twl <- readRDS(str_glue("data/gls/{id}twl.rds"))
lightraw <- readRDS(str_glue("data/gls/{id}raw.rds"))

metadata <- read_excel("data/GDR_Master_Penguin_For Movebank_1920 and 2122.xlsx") %>%
  mutate(deployed = ymd_hm(paste(`Date Deployed`, `Time Deployed (local)`)),
         `Time Recovered (local)` = strftime(`Time Recovered (local)`,
                                             "%H:%M"),
         recovered = mdy_hm(paste(`Date Recovered`, `Time Recovered (local)`))) %>%
  filter(`Tag Ref #` == id)

lon.calib <- -64.05
lat.calib <- -64.77

# Hill-Ekstrom calibration
startDate <- "2020-03-01"
endDate <- "2020-04-19"
start <- min(which(as.Date(twl$Twilight) == startDate))
end <- min(which(as.Date(twl$Twilight) == endDate))
zenith_sd <- GeoLocTools::findHEZenith(
  twl,
  tol = 0.01,
  range = c(start, end)
)


lightraw_begin <- slice(lightraw, seq(1, 1.5e6, by = 10))
lightImage(tagdata = slice(lightraw, 1:1e6),
           offset = offset,
           zlim = c(0, 20))
tsimageDeploymentLines(lightraw_begin$Date,
                       lon.calib,
                       lat.calib,
                       offset,
                       lwd = 2,
                       col = "orange")
tsimagePoints(d_calib$Twilight,
              offset,
              col = ifelse(d_calib$Rise, "firebrick", "cornflowerblue"))


abline(v = tm.calib, lwd = 2, lty = 2, col = "orange")

d_calib <- filter(twl, between(Twilight, tm.calib[1], tm.calib[2]))
calib <- thresholdCalibration(d_calib$Twilight,
                              d_calib$Rise,
                              lon.calib,
                              lat.calib,
                              method = "gamma")



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

init_pts <- st_as_sf(init_df, coords = c("lon", "lat"), crs = "epsg:4326")
init_line <- tibble(geometry = list(st_linestring(as.matrix(init_df)))) %>%
  st_as_sf(crs = "epsg:4326")

map_crs <- st_crs('+proj=laea +lon_0=-70.4882812 +lat_0=-60.5044497 +datum=WGS84 +units=m +no_defs')
map_bbox <- st_bbox(c(xmin = -100, ymin = -88, xmax = -60, ymax = -40),
                    crs = "epsg:4326") %>%
  st_as_sfc() %>%
  st_transform(map_crs) %>%
  st_bbox()

(t <- tm_shape(terra::rast("data/basemap/natgeobasemap_modified.tif")) +
    tm_rgb() +
    tm_shape(init_line, projection = map_crs, bbox = map_bbox) +
    tm_lines() +
    tm_shape(init_pts) +
    tm_symbols(col = "firebrick", size = 0.1) +
    tm_layout(title = id))

tmap_save(t, str_glue("figs/initpaths/{id}initpath.png"))
