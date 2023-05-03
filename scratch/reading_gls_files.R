library(here)
library(lubridate)
library(tidyverse)

track <- read_delim(
  here("data", "gls", "LUL_1947020.txt"),
  delim = ";",
  skip = 27,
  col_names = c("date", "time", "temp_c", "press_mbar", "lum_lux"),
  col_types = "ccddd"
) %>%
  select(-6) %>%
  mutate(datetime = dmy_hms(paste(date, time, tz = "UTC")),
         temp_c = temp_c / 10)

plot(lum_lux ~ datetime, drop_na(track, lum_lux), type = "l")

