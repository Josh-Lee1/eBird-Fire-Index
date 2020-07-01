library(tidyverse)
library(lubridate)
all_files<-list.files("filteredData",full.names = TRUE)
first_list<-list()


for (i in 1:length(all_files)){
  print(i)
  temp<-readRDS(all_files[i])
  temp$LATITUDE<-as.numeric(temp$LATITUDE)
  temp$LONGITUDE<-as.numeric(temp$LONGITUDE)
  temp$OBSERVATION.DATE<-as.character(temp$OBSERVATION.DATE)
  first_list<-bind_rows(first_list,temp)
}



first_list$in_fire <- recode(first_list$in_fire,True = "In Fire")
first_list$in_fire <- replace_na(first_list$in_fire,"Not in Fire")

table(first_list$in_fire)

first_list$OBSERVATION.DATE<-ymd(first_list$OBSERVATION.DATE)
first_list$after_fire<-first_list$OBSERVATION.DATE > ymd("2019-12-15")

table(first_list$in_fire,first_list$after_fire)

filter(first_list,after_fire&in_fire=="In Fire") %>%
  ggplot(aes(OBSERVATION.DATE))+geom_histogram()+theme_bw()
ggsave("figures/histogram_of_in_fire_samples.pdf")

