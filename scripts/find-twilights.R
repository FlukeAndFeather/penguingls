library(GeoLocTools)
library(lubridate)
library(readxl)
library(SGAT)
library(TwGeos)
library(tidyverse)

# PARAM
id <- "1947026"

metadata <- read_excel("data/GDR_Master_Penguin_For Movebank_1920 and 2122.xlsx") %>%
  mutate(deployed = ymd_hm(paste(`Date Deployed`, `Time Deployed (local)`)),
         `Time Recovered (local)` = strftime(`Time Recovered (local)`,
                                             "%H:%M"),
         recovered = mdy_hm(paste(`Date Recovered`, `Time Recovered (local)`))) %>%
  filter(`Tag Ref #` == id)
lon.calib <- switch(metadata$Island,
                    AVI = -64.1, # Where's AVI?
                    BIS = -(63 + 46 / 60))
lat.calib <- switch(metadata$Island,
                    AVI = -64.8,
                    BIS = -(64 + 49 / 60))

lightraw <- read_csv(str_glue("data/gls/{id}.csv")) %>%
  select(Date = timestamp,
         Light = `gls:light-level`) %>%
  slice(seq(1, nrow(.), by = 5)) %>%
  filter(between(Date, metadata$deployed, metadata$recovered)) %>%
  drop_na(Light)

# PARAM
threshold <- 1
offset <- 18 # adjusts the y-axis to put night (dark shades) in the middle
# (0 to 4, or 23 depending on files - no negative values!)
lightImage(tagdata = lightraw,
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
png(str_glue("figs/twilight/{id}twl.png"))
lightImage(tagdata = lightraw,
           offset = offset,
           zlim = c(0, 20))
tsimagePoints(twl$Twilight, offset = offset, pch = 16, cex = 1.2,
              col = ifelse(twl$Rise, "firebrick", "cornflowerblue"))
dev.off()
saveRDS(twl, str_glue("data/gls/{id}twl.rds"))
saveRDS(lightraw, str_glue("data/gls/{id}raw.rds"))
