### extract coefficients and se
library(tidyverse)
library(mgcv)

setwd("model_objects/")

mod<- readRDS("Australian_Magpie.RDS")
datsum<- summary(mod)
coeff<- mod$coefficients
se<- datsum$se
devi<- mod$deviance
wgoods<- data.frame(coeff, se, devi)
tibble(species=file_name, coeff=coeff[2], se=se[2])
  
all_files <- list.files("../model_objects/")

  
setwd("..")
  
  extractingcoeffse_function <- function(file_name) {
    data <- readRDS(file_name)
    datsum<- summary(data)
    coeff<- data$coefficients
    se<- datsum$se
    deviance<- data$deviance
    wgoods<- data.frame(coeff, se, deviance) 
    
    saveRDS(wgoods, file = paste0("models_extracted/", gsub("model_objects/", "",file_name), ".RDS"))
    
    return(tibble(species=file_name, coeff=coeff[2], se=se[2]))
  }
  
  bird_response_df <- map_df(all_files, extractingcoeffse_function)
  
 
  # want the function to: read file, extract bits, make df, save it in another folder. 
  # then loop for all 

  