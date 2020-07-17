### extract coefficients and se

setwd("model_objects/")

mod<- readRDS("Australian_Magpie.RDS")
datsum<- summary(mod)
coeff<- mod$coefficients
se<- datsum$se
wgoods<- data.frame(coeff, se)

  
files <- list.files("../model_objects/")

  
setwd("..")
  
  extractingcoeffse_function <- function(file_name) {
    data <- readRDS(file_name)
    datsum<- summary(data)
    coeff<- dat$coefficients
    se<- datsum$se
    wgoods<- data.frame(coeff, se) 
    
    saveRDS(wgoods, file = paste0("models_extracted/", gsub("model_objects/", "",file_name), ".RDS"))
  }

  extractingcoeffse_function(file_name)
  
  # want the function to: read file, extract bits, make df, save it in another folder. 
  # then loop for all 