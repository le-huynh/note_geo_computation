#'---
#' title: Check EAC4 data downloaded via web form
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
    rio,            # import and export files
    here,           # locate files 
    tidyverse,      # data management and visualization
    chva.extras,
    sf,
    terra
)

#' # One variable
# 1 variable #------------------
data_web1 <- terra::rast(here("eac4/eac4_va_pm25_web.nc"))

data_web1

(times <- terra::time(data_web1))

df_cleaned1 <- data_web1 %>% 
    terra::as.data.frame(xy = TRUE, time = TRUE) %>% 
    tidyr::pivot_longer(cols = c(-x, -y), names_to = "timeUTC", 
                        values_to = "value") %>% 
    mutate(timeUTC = str_match(timeUTC, "=(.*)")[,2],
           timeUTC = as.numeric(timeUTC),
           timeUTC = as_datetime(timeUTC, origin = lubridate::origin, tz = "UTC"),
           timeET = lubridate::ymd_hms(timeUTC, tz = "UTC"),
           timeET = lubridate::with_tz(timeET, "US/Eastern"), 
           .after = timeUTC) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
df_cleaned1

ggplot() +
    geom_sf(data = df_cleaned1,
            aes(geometry = geometry)) +
    geom_sf(data = chva.extras::sf_va_county,
            aes(geometry = geometry),
            fill = NA) +
    theme_bw()

#' # Two variables
# 2 variables #------------------
data_web2 <- terra::rast(here("eac4/eac4_va_pm1_pm10_web.nc"))

data_web2

terra::varnames(data_web2)

raster_pm1 <- subset(data_web2,
                     str_detect(names(data_web2), "pm1_"))
raster_pm1

raster_pm10 <- subset(data_web2,
                     str_detect(names(data_web2), "pm10_"))
raster_pm10

df_cleaned_pm10 <- raster_pm10 %>% 
    terra::as.data.frame(xy = TRUE, time = TRUE) %>% 
    tidyr::pivot_longer(cols = c(-x, -y), names_to = "timeUTC", 
                        values_to = "value") %>% 
    mutate(timeUTC = str_match(timeUTC, "=(.*)")[,2],
           timeUTC = as.numeric(timeUTC),
           timeUTC = as_datetime(timeUTC, origin = lubridate::origin, tz = "UTC"),
           timeET = lubridate::ymd_hms(timeUTC, tz = "UTC"),
           timeET = lubridate::with_tz(timeET, "US/Eastern"), 
           .after = timeUTC) %>% 
    sf::st_as_sf(coords = c("x", "y"), crs = "WGS84")
df_cleaned_pm10

ggplot() +
    geom_sf(data = df_cleaned_pm10,
            aes(geometry = geometry)) +
    geom_sf(data = chva.extras::sf_va_county,
            aes(geometry = geometry),
            fill = NA) +
    theme_bw()

