library(BAStag)
library(SGAT)
library(sf)
library(SOmap)
library(patchwork)

# See reading_gls_files.R
track_bas <- track %>%
  drop_na(lum_lux) %>%
  select(Date = datetime, Light = lum_lux)
lightImage(track_bas)

# Deployed Jan 20?
track_bas <- track %>%
  drop_na(lum_lux) %>%
  filter(datetime >= ymd_hms("2020-01-21 00:00:00", tz = "UTC")) %>%
  select(Date = datetime, Light = lum_lux)

# Palmer sunrise/sunset
palmer_ll <- c(lat = -64.7743, lon = -64.0538)
approx_twilight <- function(dates, rise_set = c("rise", "set"), tz) {
  rise_set <- match.arg(rise_set)
  ISOdate(year = year(dates),
          month = month(dates),
          day = day(dates),
          hour = ifelse(rise_set == "rise", 6, 18),
          tz = tz)
}
palmer_suntimes <- tibble(
  date = seq(min(track_bas$Date), max(track_bas$Date), by = "1 day"),
  sunrise0 = approx_twilight(date, "rise", "Etc/GMT+3"),
  sunset0 = approx_twilight(date, "set", "Etc/GMT+3"),
  sunrise = sunrise(sunrise0, palmer_ll["lon"], palmer_ll["lat"]),
  sunset = sunset(sunset0, palmer_ll["lon"], palmer_ll["lat"])
)

# Raw light data
plot(Light ~ Date, track_bas, type = "l")
abline(v = palmer_suntimes$sunrise, col = "goldenrod")
abline(v = palmer_suntimes$sunset, col = "midnightblue")

# Histogram of putative night readings
approx_posix <- function(x, y, xout, rule) {
  result <- approx(x, y, xout = xout, rule = rule, method = "constant")$y
  as.POSIXct(result, origin = "1970-01-01", tz = tz(xout))
}
foo <- track_bas %>%
  mutate(prev_sunrise = approx_posix(palmer_suntimes$sunrise,
                                     palmer_suntimes$sunrise,
                                     xout = Date,
                                     rule = 1),
         prev_sunset = approx_posix(palmer_suntimes$sunset,
                                    palmer_suntimes$sunset,
                                    xout = Date,
                                    rule = 1),
         palmer_night = prev_sunset >= prev_sunrise)

ggplot(foo, aes(Date, Light)) +
  geom_line(aes(group = 1, color = palmer_night)) +
  geom_vline(xintercept = palmer_suntimes$sunrise, color = "goldenrod") +
  geom_vline(xintercept = palmer_suntimes$sunset, color = "midnightblue") +
  scale_color_manual(values = c(`NA` = "grey",
                                `TRUE` = "midnightblue",
                                `FALSE` = "goldenrod")) +
  theme_classic()

night_light <- filter(foo, palmer_night)
light_thr <- quantile(night_light$Light, 0.95)
ggplot(night_light, aes(log(Light + 1))) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = log(light_thr + 1)) +
  theme_classic()

# Light image
lightImage(track_bas,
           offset = 0,
           zlim = c(0, light_thr),
           col = colorRampPalette(c("midnightblue", "white"))(256))
tsimageLines(palmer_suntimes$sunrise, offset = 0, col = "goldenrod", lwd = 2)
tsimageLines(palmer_suntimes$sunset, offset = 0, col = "goldenrod", lwd = 2)

# Annotate twilight
par(mfrow = c(2, 1))
# Raw light
plot(Light ~ Date, track_bas, type = "l")
abline(v = palmer_suntimes$sunrise, col = "goldenrod")
abline(v = palmer_suntimes$sunset, col = "midnightblue")
# Binned light
light_rle <- rle(track_bas$Light <= light_thr)
is_night <- rep(
  light_rle$values & light_rle$lengths >= 30 * 12,
  light_rle$lengths
)
plot(is_night ~ Date, track_bas, type = "l")
abline(v = palmer_suntimes$sunrise, col = "goldenrod")
abline(v = palmer_suntimes$sunset, col = "midnightblue")

plot(track_bas$Date[2e5:3e5], is_night[2e5:3e5], type = "l")
abline(v = palmer_suntimes$sunrise, col = "goldenrod")
abline(v = palmer_suntimes$sunset, col = "midnightblue")

# Path estimation
sunrises <- track_bas$Date[!is_night & lag(is_night, default = FALSE)]
sunrises <- sunrises[sunrises > ymd_h("2020-01-25 00", tz = "UTC")]
sunsets <- track_bas$Date[lag(!is_night, default = FALSE) & is_night]
sunsets <- sunsets[sunsets > ymd_h("2020-01-25 00", tz = "UTC")]
# drop sunrises/sunsets <12 hours after previous
diff_sunrise <- as.numeric(sunrises - lag(sunrises), unit = "hours")
diff_sunrise[1] <- 24
diff_sunset <- as.numeric(sunsets - lag(sunsets), unit = "hours")
diff_sunset[1] <- 24
sunrises <- sunrises[diff_sunrise > 12]
sunsets <- sunsets[diff_sunset > 12]
path_estimate <- thresholdEstimate(sunrises, sunsets) %>%
  as_tibble() %>%
  st_as_sf(coords = 1:2, crs = "EPSG:4326") %>%
  mutate(date = sunrises + (sunsets - sunrises) / 2,
         lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])
p1 <- ggplot(path_estimate, aes(date, lon)) +
  geom_line() +
  theme_classic()
p2 <- ggplot(path_estimate, aes(date, lat)) +
  geom_line() +
  theme_classic()
p3 <- SOmap_auto(path_estimate$lon, path_estimate$lat)
layout(matrix(c(1, 2, 1, 3), nrow = 2), heights = c(3, 2))
SOmap_auto(path_estimate$lon, path_estimate$lat)
plot(lon ~ date, path_estimate, type = "l")
plot(lat ~ date, path_estimate, type = "l")
