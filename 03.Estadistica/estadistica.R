###########################################################################

#                       Estadistica

###########################################################################
rm(list=ls())

source("07.r_scripts/LibraryRequireInstaller.r")
source('jk.modularity.R')

libraryRequireInstall("readr")
libraryRequireInstall("dplyr")
libraryRequireInstall("plyr")
libraryRequireInstall("igraph")
libraryRequireInstall("visNetwork")
libraryRequireInstall("reshape2")
libraryRequireInstall("tidyverse")



aal <- read.csv('aal_extended.csv', header=F, sep = ',')
aalnames <- aal[,2]


files <- list.files('DataSujetos')

sujetos <- list()

for(i in 1:length(files)) {
    
  sujetos[as.character(files[i])] <- list(as.matrix(read.csv(paste("./DataSujetos/",files[i], sep = ""), header = F)))
  colnames(sujetos[[i]]) <- aalnames
  rownames(sujetos[[i]]) <- aalnames
    
}


N1 <- list()
N2 <- list()
N3 <- list()
W <- list()


for(i in 1:length(sujetos)) {
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N1'){
    
    N1[i] <- as.matrix(sujetos[i])
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N2'){
    
    N2[i] <- as.matrix(sujetos[i])
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'N3'){
    
    N3[i] <- as.matrix(sujetos[i])
    
  }
  
  
  if(substr(names(sujetos)[i], start = 1, stop = 2) == 'W_'){
    
    W[i] <- as.matrix(sujetos[i])
    
  }
  
}


N1 <- compact(N1)
N2 <- compact(N2)
N3 <- compact(N3)
W <- compact(W)


############################################################


############# Louvain - Nro Comunidades ####################

N = dim(N1[[1]])[1]
Nmaxlinks = N*(N-1)

nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.Qlist_AVG = array(data=NA, dim=length(nlist))
W.Qlist_SD = array(data=NA, dim=length(nlist))
N1.Qlist_AVG = array(data=NA, dim=length(nlist))
N1.Qlist_SD = array(data=NA, dim=length(nlist))
N2.Qlist_AVG = array(data=NA, dim=length(nlist))
N2.Qlist_SD = array(data=NA, dim=length(nlist))
N3.Qlist_AVG = array(data=NA, dim=length(nlist))
N3.Qlist_SD = array(data=NA, dim=length(nlist))


k = 0
for (n in nlist) {
  k = k+1
  dlist[k] = n/Nmaxlinks
  W_temp <- c()
  N1_temp <- c()
  N2_temp <- c()
  N3_temp <- c()
  for(i in 1:18){
    W_temp[i] = jk.modularity(as.matrix(W[[i]]),n,"Louvain")[2]
    N1_temp[i] = jk.modularity(as.matrix(N1[[i]]),n,"Louvain")[2]
    N2_temp[i] = jk.modularity(as.matrix(N2[[i]]),n,"Louvain")[2]
    N3_temp[i] = jk.modularity(as.matrix(N3[[i]]),n,"Louvain")[2]
  }
  
  W.Qlist_AVG[k] = mean(W_temp)
  W.Qlist_SD[k] = sd(W_temp)
  N1.Qlist_AVG[k] = mean(N1_temp)
  N1.Qlist_SD[k] = sd(N1_temp)
  N2.Qlist_AVG[k] = mean(N2_temp)
  N2.Qlist_SD[k] = sd(N2_temp)
  N3.Qlist_AVG[k] = mean(N3_temp)
  N3.Qlist_SD[k] = sd(N3_temp)
  
}


df_comu <- data.frame(dlist,
                      W.Qlist_AVG,
                      W.Qlist_SD,
                      N1.Qlist_AVG,
                      N1.Qlist_SD,
                      N2.Qlist_AVG,
                      N2.Qlist_SD,
                      N3.Qlist_AVG,
                      N3.Qlist_SD
                      )


##Then rearrange your data frame
df_comu1 <- data.frame(dlist, W.Qlist_AVG, N1.Qlist_AVG, N2.Qlist_AVG, N3.Qlist_AVG)
df_comu1 <- df_comu1 %>% gather(estado_sujeto, media, -dlist) 

df_comu2 <- data.frame(dlist, W.Qlist_SD, N1.Qlist_SD, N2.Qlist_SD, N3.Qlist_SD)
df_comu2 <- df_comu2 %>% gather(estado_sujeto, desvio, -dlist) 

df_comu3 <- cbind(df_comu1,desvio = df_comu2$desvio)
df_comu3$estado_sujeto <- substr(df_comu3$estado_sujeto,1,nchar(df_comu3$estado_sujeto)-4)




df_comu3[df_comu3$estado_sujeto == 'W.Qlist' | df_comu3$estado_sujeto == 'N1.Qlist',] %>%
  ggplot(., aes(x = dlist, y = media, group=estado_sujeto, color=estado_sujeto)) +
    geom_line(size=1.2) +  # first layer
    geom_point() +
    geom_errorbar(aes(ymin=media-desvio, ymax=media+desvio)) +
    scale_color_brewer(palette = "Dark2") +
    labs(title="Louvain - Cantidad de Comunidades - W vs N1",x="Densidad de Aristas(d)", y = "Cantidad de Comunidades (Nc)") +
    theme_classic() +
    theme(legend.position="bottom")


df_comu3[df_comu3$estado_sujeto == 'W.Qlist' | df_comu3$estado_sujeto == 'N2.Qlist',] %>%
  ggplot(., aes(x = dlist, y = media, group=estado_sujeto, color=estado_sujeto)) +
  geom_line(size=1.2) +  # first layer
  geom_point() +
  geom_errorbar(aes(ymin=media-desvio, ymax=media+desvio)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title="Louvain - Cantidad de Comunidades - W vs N2",x="Densidad de Aristas(d)", y = "Cantidad de Comunidades (Nc)") +
  theme_classic() +
  theme(legend.position="bottom")


df_comu3[df_comu3$estado_sujeto == 'W.Qlist' | df_comu3$estado_sujeto == 'N3.Qlist',] %>%
  ggplot(., aes(x = dlist, y = media, group=estado_sujeto, color=estado_sujeto)) +
  geom_line(size=1.2) +  # first layer
  geom_point() +
  geom_errorbar(aes(ymin=media-desvio, ymax=media+desvio)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title="Louvain - Cantidad de Comunidades - W vs N3",x="Densidad de Aristas(d)", y = "Cantidad de Comunidades (Nc)") +
  theme_classic() +
  theme(legend.position="bottom")



