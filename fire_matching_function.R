library(dplyr)
library(readr)
library(sf)

# first need to get a column names vector for usage in the function

fireshape_current<- st_read("National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211/National_Indicative_Aggregated_Fire_Extent_Dataset_v20200211.shp")


baa <- read_delim("Rawdatatest/baa", "\t", 
                  escape_double = FALSE, trim_ws = TRUE)
colnames(baa)<-gsub(" ",".",colnames(baa))

column_names <- colnames(baa)


read_and_process_raw_ebird <- function(filename,fireshape=fireshape){
  #system("less -n1 RawData/ebd_AU_relApr-2020.txt 2>&1", intern = FALSE, show.output.on.console = TRUE)
  baa <- read_delim(filename, "\t", 
                    escape_double = FALSE, trim_ws = TRUE,
                    col_names = FALSE)
  
  baa <- if (baa[1,1]=="GLOBAL UNIQUE IDENTIFIER") {
    baa %>% 
      slice(2:100000)
  } else {
    baa
  }
  
  colnames(baa) <- column_names
  
  
  baatrimmed <- baa %>% 
    filter(ALL.SPECIES.REPORTED==1, EFFORT.DISTANCE.KM<10, DURATION.MINUTES<300, DURATION.MINUTES>10) %>% 
    dplyr::select(SAMPLING.EVENT.IDENTIFIER, LOCALITY.ID, LATITUDE, LONGITUDE, OBSERVATION.DATE) %>% 
    distinct()
  
  ebird_points <- st_as_sf(baatrimmed, coords=c("LONGITUDE", "LATITUDE"), crs=st_crs(fireshape))
  
  pointsin <- fireshape %>% st_join(ebird_points) %>% 
    st_set_geometry(NULL) %>% 
    dplyr::select(SAMPLING.EVENT.IDENTIFIER) %>% 
    mutate(in_fire="True")
  
  pointsinout <- baatrimmed %>%
    left_join(., pointsin)
  
  saveRDS(pointsinout,file = paste0("filteredData/", gsub("Rawdatatest/", "",filename), ".rds"))
}


all_files<-list.files("Rawdatatest",full.names = TRUE)


for(i in 1:length(all_files)){
  read_and_process_raw_ebird(filename=all_files[i],fireshape=fireshape_current)
}
