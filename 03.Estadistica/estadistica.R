###########################################################################

#                       Estadistica

###########################################################################

source("07.r_scripts/LibraryRequireInstaller.r")

libraryRequireInstall("readr")
libraryRequireInstall("dplyr")
libraryRequireInstall("plyr")



aal <- read.csv('aal_extended.csv', header=F, sep = ',')

files <- list.files('DataSujetos')

sujetos <- list()

for(i in 1:length(files)) {
    
  sujetos[as.character(files[i])] <- list(as.matrix(read.csv(paste("./DataSujetos/",files[i], sep = ""), header = F)))
    
}

N1 <- list()
N2 <- list()
N3 <- list()
W <- list()


for(i in 1:length(sujetos)) {
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N1'){
    
    N1[i] <- sujetos[i]
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N2'){
    
    N2[i] <- sujetos[i]
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N3'){
    
    N3[i] <- sujetos[i]
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'W_'){
    
    W[i] <- sujetos[i]
    
  }
  
}


N1 <- compact(N1)
N2 <- compact(N2)
N3 <- compact(N3)
W <- compact(W)



