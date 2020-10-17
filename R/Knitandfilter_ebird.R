library(tidyverse)
library(lubridate)
all_files<-list.files("Data/Processed/filteredData",full.names = TRUE)
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
first_list$after_fire<-first_list$OBSERVATION.DATE > ymd("2020-01-10")

table(first_list$in_fire,first_list$after_fire)

filter(first_list,after_fire&in_fire=="In Fire") %>%
  ggplot(aes(OBSERVATION.DATE))+geom_histogram()+theme_bw()
ggsave("figures/histogram_of_in_fire_samples.pdf")



fireshape<- st_read("National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211/National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211.shp")


map<- filter(first_list, first_list$in_fire == "In Fire")

map2 <- st_as_sf(map, coords=c("LONGITUDE", "LATITUDE"), crs=st_crs(fireshape))



base<- sp::plot(ne_countries(country = 'Australia', scale = 'medium'))

library(rnaturalearth)
library(sf)
library(ggplot2)


aus <- ne_countries(country="Australia", returnclass="sf")

auscrop <- st_crop(aus, xmin = 110, xmax = 155,
                        ymin = 45, ymax = 25)


ggplot()+
  geom_sf(data=aus, fill="orange")


Mapby3<- ggplot() + 
  geom_sf(data = aus) +
  geom_sf(data = fireshape, size = 0.1, color = "Red", fill = "Red") + 
  geom_sf(data=map2, size=0.5, colour = 'Blue')+
  ggtitle("Checklists inside Fire Boundary") + 
  coord_sf() +
  theme_bw() + 
  geom_sf(data = aus)

make_map <- ggplot()+
  geom_sf(data=aus)+
  geom_sf(data=fireshape, size = 0.1, colour = "Red", fill = "Red")+
  geom_sf(data=map2, size=0.5, colour = 'Blue')+
  xlim(110, 155)+
  ylim(45, 25)+
  coord_sf() +
  theme_bw() +
  ggtitle("Checklists inside Fire Boundary")+ 
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c('Blue', 'Red'),
                    labels = c('Points', 'Fire'))


print(make_map)



