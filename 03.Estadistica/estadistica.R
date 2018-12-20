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
  W.mlist[k] = jk.modularity(W,n,"Louvain")[2]
  N1.mlist[k] = jk.modularity(N1,n,"Louvain")[2]
  N2.mlist[k] = jk.modularity(N2,n,"Louvain")[2]
  N3.mlist[k] = jk.modularity(N3,n,"Louvain")[2]
  
  ##  Grafo Random
  random_graph <- sample_gnm(116,n)
  random_louvain <- cluster_louvain(random_graph)
  Random.mlist[k] <- length(random_louvain)
}


df_comu <- data.frame(dlist,W.mlist,N1.mlist,N2.mlist,N3.mlist,Random.mlist)

color <- brewer.pal(5, name = 'Dark2')

##Then rearrange your data frame

dd_comu = melt(df_comu, id=c("dlist"))

ggplot(dd_comu, aes(x = dlist, y = value, group=variable, color=variable)) +                    # basic graphical object
  geom_line(size=1.2) +  # first layer
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(c("solid","solid","solid","solid","dashed")) +
  labs(title="Louvain - Cantidad de Comunidades",x="Densidad de Aristas(d)", y = "Cantidad de Comunidades (Nc)") +
  theme_classic() +
  theme(legend.position="bottom")





