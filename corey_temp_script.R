library(tidyverse)
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



read_dat_function <- function(file_name) {
  
  dat <- readRDS(file_name)
  
  return(dat)
}

files <- list.files("../rawspeciesdata/")

data <- lapply(files, read_dat_function)

data_df <- do.call(rbind, data)

length(unique(data_df$SAMPLING.EVENT.IDENTIFIER))

length(unique(data_df$COMMON.NAME))

species_count <- data_df %>%
  group_by(COMMON.NAME) %>%
  summarize(N=n())


sl <- data_df %>%
  dplyr::filter(COMMON.NAME == "Superb Lyrebird") %>%
  mutate(present=1)

sl_lists <- sl %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
  distinct()

lists_without <- data_df %>%
  dplyr::filter(! SAMPLING.EVENT.IDENTIFIER %in% sl_lists$SAMPLING.EVENT.IDENTIFIER) %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER, EFFORT.DISTANCE.KM, DURATION.MINUTES, OBSERVATION.DATE) %>% 
  distinct() %>%
  mutate(present=0)

final_sl_dat <- sl %>%
  bind_rows(lists_without)



#### check if we have lost checklists and dates...