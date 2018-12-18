rm(list=ls())

library(igraph)
library(corrplot)
library(tidyverse)
library(readxl)
library(qgraph)
library(reshape)
library(GGally)
library(ggcorrplot)
library(ggpubr)

setwd("/Volumes/Macintosh HD/Users/Mario/Dropbox/Maestria Data Minin/DM CyT/TP2/DataSujetos+AAL/DataSujetos")
setwd("E:/MARIO/Dropbox/Maestria Data Minin/DM CyT/TP2/DataSujetos+AAL/DataSujetos")


N1_suj<-list.files( pattern="(N1).*\\.csv$")
N2_suj<-list.files( pattern="(N2).*\\.csv$")
N3_suj<-list.files( pattern="(N3).*\\.csv$")
W_suj<-list.files( pattern="(W).*\\.csv$")


N1_sujs<-map(N1_suj,read.csv, header=FALSE)
N2_sujs<-map(N2_suj,read.csv, header=FALSE)
N3_sujs<-map(N3_suj,read.csv, header=FALSE)
W_sujs<-map(W_suj,read.csv, header=FALSE)


N1promed<-Reduce("+", N1_sujs) / length(N1_sujs)
N2promed<-Reduce("+", N2_sujs) / length(N1_sujs)
N3promed<-Reduce("+", N3_sujs) / length(N1_sujs)
Wpromed<-Reduce("+", W_sujs) / length(N1_sujs)

aal <- read.csv("aal_extended.csv", header = F)
aalnames <- aal[,2] 

##
N1 <- as.matrix(N1promed)
N2 <- as.matrix(N2promed)
N3 <- as.matrix(N3promed)
W <- as.matrix(Wpromed)

colnames(N1) <- aalnames 
colnames(N2) <- aalnames 
colnames(N3) <- aalnames 
colnames(W) <- aalnames 
# 
rownames(N1) <- aalnames 
rownames(N2) <- aalnames 
rownames(N3) <- aalnames 
rownames(W) <- aalnames 

## Cij
corrplot(N1, is.corr=TRUE, title = "N1", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(N2, is.corr=TRUE, title = "N2", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(N3, is.corr=TRUE, title = "N3", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(W, is.corr=TRUE, title = "W", order="hclust", cl.cex=1, tl.cex=0.4)


#graficar pesados
netN1 <- graph.adjacency(N1,mode="undirected",weighted= TRUE, diag = FALSE)
eN1 <- get.edgelist(netN1, names=FALSE)
l1 <- qgraph.layout.fruchtermanreingold(eN1,vcount=vcount(netN1),area=100*(vcount(netN1)^2),repulse.rad=(vcount(netN1)^3.1))
E(netN1)$color = rgb(0,0,0,.1)

V(netN1)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netN1, main="Visualizacion de  la estructura de la red pesada N1",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netN1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -.1,layout=l1, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netN2 <- graph.adjacency(N2,mode="undirected",weighted= TRUE, diag = FALSE)
eN2 <- get.edgelist(netN2, names=FALSE)
l2 <- qgraph.layout.fruchtermanreingold(eN2,vcount=vcount(netN2),area=100*(vcount(netN2)^2),repulse.rad=(vcount(netN2)^3.1))
E(netN2)$color = rgb(0,0,0,.1)

V(netN2)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netN2, main="Visualizacion de  la estructura de la red pesada N2",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netN1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=l2, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netN3 <- graph.adjacency(N3,mode="undirected",weighted= TRUE, diag = FALSE)
eN3 <- get.edgelist(netN3, names=FALSE)
l3 <- qgraph.layout.fruchtermanreingold(eN3,vcount=vcount(netN3),area=100*(vcount(netN3)^2),repulse.rad=(vcount(netN3)^3.1))
E(netN3)$color = rgb(0,0,0,.1)

V(netN3)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netN3, main="Visualizacion de  la estructura de la red pesada N3",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netN1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=l3, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netW <- graph.adjacency(W,mode="undirected",weighted= TRUE, diag = FALSE)
eW <- get.edgelist(netN3, names=FALSE)
lW <- qgraph.layout.fruchtermanreingold(eW,vcount=vcount(netW),area=100*(vcount(netW)^2),repulse.rad=(vcount(netW)^3.1))
E(netW)$color = rgb(0,0,0,.1)

V(netw)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netW, main="Visualizacion de  la estructura de la red pesada W",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netN1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=lW, vertex.size = 10, vertex.label.cex = 0.6)

## Umbral de correcion en funcion de la densidad de aristas

diag(N1)<-0
diag(N2)<-0
diag(N3)<-0
diag(W)<-0

N = dim(N1)[1]
Nmaxlinks = N*(N-1)
out <- data.frame(matrix(ncol = 5, nrow = (Nmaxlinks*0.4)))
colnames(out)<-c("Delta", "N1", "N2", "N3", "W")

for(i in 0:(Nmaxlinks*0.4)){
  n = i
  delta = n/Nmaxlinks
  tmp1<-sort(as.vector(N1),decreasing = TRUE)
  tmp2<-sort(as.vector(N2),decreasing = TRUE)
  tmp3<-sort(as.vector(N3),decreasing = TRUE)
  tmpW<-sort(as.vector(W),decreasing = TRUE)
  ro1 = tmp1[n]
  ro2 = tmp2[n]
  ro3 = tmp3[n]
  roW = tmpW[n]
  
  out[i,1]<-delta
  out[i,2]<-ro1
  out[i,3]<-ro2
  out[i,4]<-ro3
  out[i,5]<-roW
}

ggplot(melt(out, id.vars = "Delta"), aes (x=Delta,y=value,color=variable))+
  geom_point(size=0.2, shape=21)+
  ggtitle("Umbral de corte en funcion de la densidad de aristas") +
  xlab("δ densidad de aristas") + ylab("ρ umbral de correlacion")

##########GENERACION DE GRAFOS NO PESADOS CON DISTINTOS VALORES DE DENSIDAD DE ARISTAS#######

###Creo matrices no pesadas para un intervalo de densidad de arista 0.025 0.15 incrementos de 0.005
deltas<-seq(from=0.025, to=0.15, by=0.005)
ns<-deltas*Nmaxlinks

ros.N1 <- tmp1[ns]
ros.N2 <- tmp2[ns]
ros.N3 <- tmp3[ns]
ros.W <- tmpW[ns]

N1b <- data.frame(V1=logical(116))
N2b <- data.frame(V1=logical(116))
N3b <- data.frame(V1=logical(116))
Wb <- data.frame(V1=logical(116))

for (i in 1:length(ros.N1)){ 
  N1b[i] = N1>ros.N1[i]
}
for (i in 1:length(ros.N2)){ 
  N2b[i] = N2>ros.N2[i]
}
for (i in 1:length(ros.N3)){ 
  N3b[i] = N3>ros.N3[i]
}
for (i in 1:length(ros.W)){ 
  Wb[i] = W>ros.W[i]
}

netN1<-N1b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netN2<-N2b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netN3<-N3b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netW<-Wb %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))

plotsN1b<-N1b %>% map(~ ggcorrplot(.x, type = "upper", title = "N1 thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsN2b<-N2b %>% map(~ ggcorrplot(.x, type = "upper", title = "N2 thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsN3b<-N3b %>% map(~ ggcorrplot(.x, type = "upper", title = "N3 thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsWb<-Wb %>% map(~ ggcorrplot(.x, type = "upper", title = "W thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))

ggarrange(plotlist=plotsN1b)
ggarrange(plotlist=plotsN2b)
ggarrange(plotlist=plotsN3b)
ggarrange(plotlist=plotsWb)



################### CENTRALIDAD ##################

################### GRADO ##################

################### CAMINO MINIMO ##################

################### COEFICIENTE DE CLUSTERING ##################



