### extract coefficients and se
library(tidyverse)
library(mgcv)


mod<- readRDS("Australian_Magpie.RDS")
datsum<- summary(mod)
coeff<- mod$coefficients
se<- datsum$se
devi<- mod$deviance
wgoods<- data.frame(coeff, se, devi)
tibble(species=file_name, coeff=coeff[2], se=se[2])
  
all_files <- list.files("model_objects/")

  

  
  extractingcoeffse_function <- function(file_name) {
    data <- readRDS(paste0("model_objects/", file_name))
    datsum<- summary(data)
    #coeff<- data$coefficients
    se<- datsum$se
    deviance<- datsum$dev.expl
    n<-datsum$n
    wgoods<- as.data.frame(summary(data)$p.tab) %>%
      rownames_to_column(var="Term") %>%
      mutate(deviance=deviance) %>%
      mutate(N=n) %>%
      mutate(converged=data$converged) %>%
      mutate(species=gsub(".RDS", "", file_name))
    
    #saveRDS(wgoods, file = paste0("models_extracted/", gsub("model_objects/", "",file_name), ".RDS"))
    
    return(wgoods)
  }
  
  bird_response_df <- map_df(all_files, extractingcoeffse_function)
  
 
  # want the function to: read file, extract bits, make df, save it in another folder. 
  # then loop for all 

  