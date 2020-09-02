library(raster)

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
  filter(n>75)

m<-group_by(final_sl_dat,COMMON.NAME)%>%
  filter(sev >0) %>%
  summarize(median.sev=median(sev))

final_sl_dat %>%
  filter(COMMON.NAME %in% z$COMMON.NAME) %>%
  filter(sev >0) %>%
  left_join(m) %>%
ggplot(aes(x=reorder(COMMON.NAME, -median.sev),y=sev))+coord_flip()+geom_boxplot()+
  ylab("Average severity of area around checklist (larger number=more severe)")+
  xlab("")

