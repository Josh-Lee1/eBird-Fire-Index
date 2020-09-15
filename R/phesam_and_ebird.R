library(raster)
library(lubridate)

setwd("~/Documents/eBird-Fire-Index/")

data_df<-read_csv("processed_data/ebird_data_with_fire_dates.csv")


final_sl_dat <- data_df %>%
  #  bind_rows(lists_without) %>%
  dplyr::filter(!is.na(day_of_fire)) %>%
  mutate(DURATION.MINUTES=as.numeric(DURATION.MINUTES),
         EFFORT.DISTANCE.KM=as.numeric(EFFORT.DISTANCE.KM)) %>%
  mutate(MONTH=month(OBSERVATION.DATE)) %>%
  mutate(before.after=ifelse(OBSERVATION.DATE>day_of_fire, "After", "Before")) %>%
  filter(before.after=="After")



fesm1000<-raster("Data/Raw/fesm_geotiff/fesm1000.tif")
b<-SpatialPointsDataFrame(cbind(final_sl_dat$LONGITUDE,final_sl_dat$LATITUDE), final_sl_dat, match.ID = FALSE,
                          proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
bb<-spTransform(b,crs(fesm1000))
final_sl_dat$sev<-extract(fesm1000,bb)

ggplot(final_sl_dat,aes(x=COMMON.NAME,y=sev))+coord_flip()+geom_point()

z<-group_by(final_sl_dat,COMMON.NAME) %>%
  summarize(n=n()) %>%
  filter(n>50)

m<-group_by(final_sl_dat,COMMON.NAME)%>%
  filter(sev >0) %>%
  summarize(median.sev=median(sev,na.rm=T))

final_sl_dat %>%
  filter(COMMON.NAME %in% z$COMMON.NAME) %>%
  filter(sev >0) %>%
  left_join(m) %>%
ggplot(aes(x=reorder(COMMON.NAME, -median.sev),y=sev))+coord_flip()+geom_boxplot()+
  ylab("Average severity of area around checklist (larger number=more severe)")+
  xlab("")

valuesfin$foodspec




vf<-valuesfin %>%
  mutate(feeding_specialisation=ifelse(foodspec>1,"generalist","specialist"))

bird_response_df %>%
  filter(Term=="before.afterBefore") %>%
  mutate(COMMON.NAME=gsub("_"," ",species)) %>%
  left_join(m) %>%
  left_join(vf)->oo


write_csv(oo,"processed_data/severity_data_and_traits.csv")



library(readr)
library(plotly)
bird_response_df<-read_csv("processed_data/severity_data_and_traits.csv")

bird_respones_new <- dplyr::filter(oo, !(bird_response_df$species == "Australian_Pelican" | 
                                                         bird_response_df$species == "Chestnut_Teal" |
                                                         bird_response_df$species == "Great_Cormorant"|
                                                         bird_response_df$species == "Little_Black_Cormorant"|
                                                         bird_response_df$species == "Little_Pied_Cormorant"|
                                                         bird_response_df$species == "Maned_Duck"|
                                                         bird_response_df$species == "Pacific_Black_Duck"|
                                                         bird_response_df$species == "Silver_Gull"|
                                                         bird_response_df$species == "White-faced_Heron"))


z<-ggplot(bird_respones_new,aes(x=median.sev,y=-1*Estimate,label=COMMON.NAME,))+
  geom_point(aes(col=feeding_specialisation))+geom_smooth(method="lm")+ylab("Modelled Response to Fire")+
  geom_hline(yintercept = 0,linetype="dashed")+xlab("Median fire severity in post-fire observations")+
  theme_classic()
ggsave("Output/figures/severity_response.pdf")


ggplotly(z)

summary(lm(Estimate~median.sev,data=bird_respones_new))

