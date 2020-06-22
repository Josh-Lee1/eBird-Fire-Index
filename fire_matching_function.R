



read_and_process_raw_ebird <- function(filename,fireshape=fireshape){
  #system("less -n1 RawData/ebd_AU_relApr-2020.txt 2>&1", intern = FALSE, show.output.on.console = TRUE)
  baa<- read.delim(paste0("RawData/",filename), header = TRUE, sep="\t")
  baatrimmed<- baa %>% 
    filter(ALL.SPECIES.REPORTED==1, EFFORT.DISTANCE.KM<10, DURATION.MINUTES<300, DURATION.MINUTES>10) %>% 
    select(SAMPLING.EVENT.IDENTIFIER, LOCALITY.ID, LATITUDE, LONGITUDE, OBSERVATION.DATE) %>% 
    distinct()
  
  ebird_points <- st_as_sf(baatrimmed, coords=c("LONGITUDE", "LATITUDE"), crs=st_crs(fireshape))
  pointsinout<-st_join(ebird_points, fireshape, join = st_within)
  saveRDS(pointsinout,file = paste0("filteredData/",filename,".rds"))
}





fireshape_current<- st_read("National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211/National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211.shp")
read_and_process_raw_ebird(filename="baa.csv",fireshape=fireshape_current)



