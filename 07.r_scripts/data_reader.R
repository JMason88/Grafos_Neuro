###########################################################################

#                       Data Reader

###########################################################################

source("./07.r_scripts/LibraryRequireInstaller.r")
rm(list = ls())

libraryRequireInstall("readr")

data_reader <- function(x="N1") {
  
  aal <- read.csv('aal_extended.csv', header=F, sep = ',')
  
  files <- list.files('DataSujetos')
  
  sujetos <- list()
  
  for(i in 1:length(files)) {
    
    if(substr(files[i], start = 1, stop = 2) == x){
      
      sujetos[as.character(files[i])] <- list(as.matrix(read.csv(paste("./DataSujetos/",files[i], sep = ""), header = F)))
      
    }
    
  }
  
  promedio <- Reduce('+', sujetos)/length(sujetos)
  
  
  return(promedio)
}


