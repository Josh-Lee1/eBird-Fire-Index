library(tidyverse)
library(lubridate)
setwd("rawspeciesdata/")

read_dat_function <- function(file_name) {
  dat <- readRDS(file_name)
  dat$OBSERVATION.DATE<-ymd(dat$OBSERVATION.DATE)
  return(dat)
}

files <- list.files("../rawspeciesdata/")

data <- lapply(files, read_dat_function)
data_df <- do.call(rbind, data)
data_df$LATITUDE<-as.numeric(data_df$LATITUDE)
data_df<-filter(data_df,LATITUDE < -25)


hot <- read_csv("../DEA_hotspots/east_aus_hotspots.csv")


get_hottest <- function(int_ss, hot = hot) {
  ohot <- slice(hot, int_ss)
  if (!is_tibble(ohot)) {
    return(NA)
  }
  return(max(ohot$temp_kelvin, na.rm = TRUE))
}

get_powerest <- function(int_ss, hot = hot) {
  ohot <- slice(hot, int_ss)
  if (!is_tibble(ohot)) {
    return(NA)
  }
  return(max(ohot$power, na.rm = TRUE))
}

get_earliest <- function(int_ss, hot = hot) {
  ohot <- slice(hot, int_ss)
  if (!is_tibble(ohot)) {
    return(NA)
  }
  return(as.character(min(ymd(substr(ohot$datetime, 1, 10)), na.rm = TRUE)))
}


#buffer stage
create_buffer <- function(buffer) {
  pts     <- sf::st_as_sf(data_df,
                          coords = c("LONGITUDE", "LATITUDE"),
                          remove = F)
  pts_buf <- sf::st_buffer(pts, buffer)
  pts2 <-
    sf::st_as_sf(hot,
                 coords = c("longitude", "latitude"),
                 remove = F)
  int <- sf::st_intersects(pts_buf, pts2, sparse = T)
  
  data_df$temperature <- sapply(int, get_hottest, hot)
  data_df$power <- sapply(int, get_powerest, hot)
  data_df$day_of_fire <- sapply(int, get_earliest, hot)

  return(data_df)
}

out<-sapply(seq(0.005, 0.02, 0.001), create_buffer)

buffer<-0.05



data_df %>%
  filter(LONGITUDE>135&!is.na(day_of_fire)) %>%
ggplot(
       aes(x=LONGITUDE,y=LATITUDE))+geom_point(aes(color = day_of_fire))

write_csv(data_df,"../processed_data/ebird_data_with_fire_dates.csv")


