rm(list=ls())

source('07.r_scripts/LibraryRequireInstaller.r')
source('07.r_scripts/data_reader.r')
source('jk.modularity.R')

libraryRequireInstall('igraph')
libraryRequireInstall('visNetwork')
libraryRequireInstall('ggplot2')
libraryRequireInstall('RColorBrewer')
libraryRequireInstall("reshape2")

####################### Data Loading #######################

N1 <- data_reader(x = "N1")
N2 <- data_reader(x = "N2")
N3 <- data_reader(x = "N3")
W <- data_reader(x = "W_")

############################################################


############# Louvain - Nro Comunidades ####################

N = dim(N1)[1]
Nmaxlinks = N*(N-1)

nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.mlist = array(data=NA, dim=length(nlist))
N1.mlist = array(data=NA, dim=length(nlist))
N2.mlist = array(data=NA, dim=length(nlist))
N3.mlist = array(data=NA, dim=length(nlist))
Random.mlist = array(data=NA, dim = length(nlist))

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




################## Louvain Modularity ########################

N = dim(N1)[1]
Nmaxlinks = N*(N-1)

nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.mlist = array(data=NA, dim=length(nlist))
N1.mlist = array(data=NA, dim=length(nlist))
N2.mlist = array(data=NA, dim=length(nlist))
N3.mlist = array(data=NA, dim=length(nlist))
Random.mlist = array(data=NA, dim = length(nlist))

k = 0
for (n in nlist) {
  k = k+1
  dlist[k] = n/Nmaxlinks
  W.mlist[k] = jk.modularity(W,n,"Louvain")[1]
  N1.mlist[k] = jk.modularity(N1,n,"Louvain")[1]
  N2.mlist[k] = jk.modularity(N2,n,"Louvain")[1]
  N3.mlist[k] = jk.modularity(N3,n,"Louvain")[1]
  
  ##  Grafo Random
  random_graph <- sample_gnm(116,n)
  random_louvain <- cluster_louvain(random_graph)
  Random.mlist[k] <- modularity(random_graph, random_louvain$membership)
}


df <- data.frame(dlist,W.mlist,N1.mlist,N2.mlist,N3.mlist,Random.mlist)

color <- brewer.pal(5, name = 'Dark2')

##Then rearrange your data frame
dd = melt(df, id=c("dlist"))

ggplot(dd, aes(x = dlist, y = value, group=variable, color=variable)) +                    # basic graphical object
  geom_line(size=1.2) +  # first layer
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(c("solid","solid","solid","solid","dashed")) +
  labs(title="Louvain - Coeficiente de Modularidad",x="Densidad de Aristas(d)", y = "Coeficiente de Modularidad (Q)") +
  theme_classic() +
  theme(legend.position="bottom")


############# Girvan-Newman Nro Comunidades ####################

N = dim(N1)[1]
Nmaxlinks = N*(N-1)

nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.mlist = array(data=NA, dim=length(nlist))
N1.mlist = array(data=NA, dim=length(nlist))
N2.mlist = array(data=NA, dim=length(nlist))
N3.mlist = array(data=NA, dim=length(nlist))
Random.mlist = array(data=NA, dim = length(nlist))

k = 0
for (n in nlist) {
  k = k+1
  dlist[k] = n/Nmaxlinks
  W.mlist[k] = jk.modularity(W,n,"Girvan-Newman")[2]
  N1.mlist[k] = jk.modularity(N1,n,"Girvan-Newman")[2]
  N2.mlist[k] = jk.modularity(N2,n,"Girvan-Newman")[2]
  N3.mlist[k] = jk.modularity(N3,n,"Girvan-Newman")[2]
  
  ##  Grafo Random
  random_graph <- sample_gnm(116,n)
  random_louvain <- cluster_edge_betweenness(random_graph)
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
  labs(title="Girvan-Newman - Cantidad de Comunidades",x="Densidad de Aristas(d)", y = "Cantidad de Comunidades (Nc)") +
  theme_classic() +
  theme(legend.position="bottom")




################## Girvan-Newman Modularity ########################

N = dim(N1)[1]
Nmaxlinks = N*(N-1)

nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.mlist = array(data=NA, dim=length(nlist))
N1.mlist = array(data=NA, dim=length(nlist))
N2.mlist = array(data=NA, dim=length(nlist))
N3.mlist = array(data=NA, dim=length(nlist))
Random.mlist = array(data=NA, dim = length(nlist))

k = 0
for (n in nlist) {
  k = k+1
  dlist[k] = n/Nmaxlinks
  W.mlist[k] = jk.modularity(W,n,"Girvan-Newman")[1]
  N1.mlist[k] = jk.modularity(N1,n,"Girvan-Newman")[1]
  N2.mlist[k] = jk.modularity(N2,n,"Girvan-Newman")[1]
  N3.mlist[k] = jk.modularity(N3,n,"Girvan-Newman")[1]
  
  ##  Grafo Random
  random_graph <- sample_gnm(116,n)
  random_louvain <- cluster_edge_betweenness(random_graph)
  Random.mlist[k] <- modularity(random_graph, random_louvain$membership)
}


df <- data.frame(dlist,W.mlist,N1.mlist,N2.mlist,N3.mlist,Random.mlist)

color <- brewer.pal(5, name = 'Dark2')

##Then rearrange your data frame
dd = melt(df, id=c("dlist"))

ggplot(dd, aes(x = dlist, y = value, group=variable, color=variable)) +                    # basic graphical object
  geom_line(size=1.2) +  # first layer
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(c("solid","solid","solid","solid","dashed")) +
  labs(title="Girvan-Newman - Coeficiente de Modularidad",x="Densidad de Aristas(d)", y = "Coeficiente de Modularidad (Q)") +
  theme_classic() +
  theme(legend.position="bottom")
