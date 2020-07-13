library(tidyverse)
<<<<<<< HEAD
library(tidyverse)
=======
>>>>>>> 590cec9b91a9b7ed7fe864f6293877021512a453
library(lubridate)
setwd("rawspeciesdata/")

read_dat_function <- function(file_name) {
  dat <- readRDS(file_name)
  dat$OBSERVATION.DATE<-ymd(dat$OBSERVATION.DATE)
<<<<<<< HEAD
  return(dat)
}

files <- list.files("../rawspeciesdata/")
data <- lapply(files, read_dat_function)



read_dat_function <- function(file_name) {
  
  dat <- readRDS(file_name)
  
=======
>>>>>>> 590cec9b91a9b7ed7fe864f6293877021512a453
  return(dat)
}

files <- list.files("../rawspeciesdata/")

data <- lapply(files, read_dat_function)
data_df <- do.call(rbind, data)
data_df$LATITUDE<-as.numeric(data_df$LATITUDE)
data_df<-filter(data_df,LATITUDE < -25)

length(unique(data_df$SAMPLING.EVENT.IDENTIFIER))
length(unique(data_df$COMMON.NAME))

species_count <- data_df %>%
  group_by(COMMON.NAME) %>%
  summarize(N=n())


process<-function(species_name,data_df=data_df){
  sl <- data_df %>%
    dplyr::filter(COMMON.NAME == species_name) %>%
    mutate(present=1)
  
  sl_lists <- sl %>%
    dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>%
    distinct()
  
  lists_without <- data_df %>%
    dplyr::filter(! SAMPLING.EVENT.IDENTIFIER %in% sl_lists$SAMPLING.EVENT.IDENTIFIER) %>%
    dplyr::select(SAMPLING.EVENT.IDENTIFIER,OBSERVATION.DATE) %>% 
    distinct() %>%
    mutate(present=0)
  
  final_sl_dat <- sl %>%
    bind_rows(lists_without)
  
  final_sl_dat$before.after<-final_sl_dat$OBSERVATION.DATE>ymd("2020-01-10")
  
  final_sl_dat %>%
    group_by(before.after) %>%
    summarize(percent_observed=sum(present)/n())->out
    return(out)
}

data_df %>%
  group_by(COMMON.NAME) %>%
  count(num.obs=n()) %>%
  filter(num.obs>500)->a

<<<<<<< HEAD
lists_without <- data_df %>%
  dplyr::filter(! SAMPLING.EVENT.IDENTIFIER %in% sl_lists$SAMPLING.EVENT.IDENTIFIER) %>%
  dplyr::select(SAMPLING.EVENT.IDENTIFIER, EFFORT.DISTANCE.KM, DURATION.MINUTES, OBSERVATION.DATE) %>% 
  distinct() %>%
  mutate(present=0)

final_sl_dat <- sl %>%
  bind_rows(lists_without)



#### check if we have lost checklists and dates...
=======
setNames(as.list(a$COMMON.NAME),a$COMMON.NAME) %>% #need this weird trick to keep the species names
  map_df(process,data_df,.id="var") -> out

out %>%
  spread(key="before.after",value="percent_observed") %>%
  mutate(percentage_drop=(`FALSE`-`TRUE`)) %>%
  arrange(desc(percentage_drop))
>>>>>>> 590cec9b91a9b7ed7fe864f6293877021512a453
