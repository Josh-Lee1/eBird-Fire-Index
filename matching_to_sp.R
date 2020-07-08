# Filtering out checklists outside of fire
## Using code from knit_together.R

library(tidyverse)
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

list_infire<- filter(first_list, in_fire == "In Fire")
baa <- read_delim("RawData/baa", "\t", escape_double = FALSE, trim_ws = TRUE)
colnames(baa)<-gsub(" ",".",colnames(baa))
column_names <- colnames(baa)
baa_inside<- semi_join(baa, list_infire, by = "SAMPLING.EVENT.IDENTIFIER")


## function

extract_species_data <- function(filename, list_infire){
  baa <- read_delim(filename, "\t", 
                    escape_double = FALSE, trim_ws = TRUE,
                    col_names = FALSE)
  
  baa <- if (baa[1,1]=="GLOBAL.UNIQUE.IDENTIFIER") {
    baa %>% 
      slice(2:100000)
  } else {
    baa
  }
  
  colnames(baa) <- column_names
  
  
  baa_inside<- semi_join(baa, list_infire, by = "SAMPLING.EVENT.IDENTIFIER")
  
  
  saveRDS(baa_inside,file = paste0("rawspeciesdata/", gsub("RawData/", "",filename), ".rds"))
}


all_files<-list.files("RawData",full.names = TRUE)


for(i in 1:length(all_files)){
  extract_species_data(filename=all_files[i], list_infire)
}
