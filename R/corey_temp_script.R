
library(tidyverse)
library(lubridate)
setwd("data/Raw/rawspeciesdata")

read_dat_function <- function(file_name) {
  dat <- readRDS(file_name)
  dat$OBSERVATION.DATE<-ymd(dat$OBSERVATION.DATE)
  return(dat)
}

files <- list.files()

data <- lapply(files, read_dat_function)
data_df <- do.call(rbind, data)
data_df$LATITUDE<-as.numeric(data_df$LATITUDE)
data_df<-filter(data_df,LATITUDE < -25)

length(unique(data_df$SAMPLING.EVENT.IDENTIFIER))
length(unique(data_df$COMMON.NAME))

species_count <- data_df %>%
  group_by(COMMON.NAME) %>%
  summarize(N=n())

#setwd("..")

process<-function(species_name,data_df=data_df){
  sl <- data_df %>%
    dplyr::filter(COMMON.NAME == species_name) %>%
    mutate(present=1)
  
  sl_lists <- sl %>%
    dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
    distinct()
  
  lists_without <- data_df %>%
    dplyr::filter(! SAMPLING.EVENT.IDENTIFIER %in% sl_lists$SAMPLING.EVENT.IDENTIFIER) %>%
    dplyr::select(SAMPLING.EVENT.IDENTIFIER,OBSERVATION.DATE, DURATION.MINUTES, LATITUDE, LONGITUDE,
                  EFFORT.DISTANCE.KM, COUNTY.CODE, OBSERVER.ID,day_of_fire) %>% 
    distinct() %>%
    mutate(present=0)
  
  final_sl_dat <- sl %>%
    bind_rows(lists_without) %>%
    filter(!is.na(day_of_fire)) %>%
    mutate(DURATION.MINUTES=as.numeric(DURATION.MINUTES),
           EFFORT.DISTANCE.KM=as.numeric(EFFORT.DISTANCE.KM)) %>%
    mutate(MONTH=month(OBSERVATION.DATE)) %>%
    mutate(before.after=ifelse(OBSERVATION.DATE>day_of_fire, "After", "Before"))
  
  mod <- mgcv::gam(present ~ before.after + s(DURATION.MINUTES) +
                     s(EFFORT.DISTANCE.KM) + s(MONTH, bs="cc", k=11), 
                   family="binomial", data=final_sl_dat)
  
  saveRDS(mod, paste0("Output/data/model_objects/", gsub(" ", "_", species_name), ".RDS"))
  
  final_sl_dat %>%
    group_by(before.after) %>%
    summarize(percent_observed=sum(present)/n())->out
  return(out)
}

data_df_with_date<-read.csv("processed_data/ebird_data_with_fire_dates.csv")

data_df_with_date %>%
  group_by(COMMON.NAME) %>%
  count(num.obs=n()) %>%
  filter(num.obs>500)->a

setNames(as.list(a$COMMON.NAME),a$COMMON.NAME) %>% #need this weird trick to keep the species names
  map_df(process,data_df_with_date,.id="var") -> out

drop<- out %>%
  spread(key="before.after",value="percent_observed") %>%
  mutate(percentage_drop=(`After`-`Before`)) %>%
  arrange(percentage_drop)

