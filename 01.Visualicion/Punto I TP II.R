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
library(gridExtra)
library(grid)
library(RColorBrewer)

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
NM1 <- as.matrix(N1promed)
NM2 <- as.matrix(N2promed)
NM3 <- as.matrix(N3promed)
MW <- as.matrix(Wpromed)

colnames(NM1) <- aalnames 
colnames(NM2) <- aalnames 
colnames(NM3) <- aalnames 
colnames(MW) <- aalnames 
# 
rownames(NM1) <- aalnames 
rownames(NM2) <- aalnames 
rownames(NM3) <- aalnames 
rownames(MW) <- aalnames 

## Cij
corrplot(NM1, is.corr=TRUE, title = "N1", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(NM2, is.corr=TRUE, title = "N2", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(NM3, is.corr=TRUE, title = "N3", order="hclust", cl.cex=1, tl.cex=0.4)
corrplot(MW, is.corr=TRUE, title = "W", order="hclust", cl.cex=1, tl.cex=0.4)


#graficar pesados
netNM1 <- graph.adjacency(NM1,mode="undirected",weighted= TRUE, diag = FALSE)
eNM1 <- get.edgelist(netNM1, names=FALSE)
l1 <- qgraph.layout.fruchtermanreingold(eNM1,vcount=vcount(netNM1),area=100*(vcount(netNM1)^2),repulse.rad=(vcount(netNM1)^3.1))
E(netNM1)$color = rgb(0,0,0,.1)

V(netNM1)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netNM1, main="Visualizacion de  la estructura de la red pesada N1",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netNM1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -.1,layout=l1, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netNM2 <- graph.adjacency(NM2,mode="undirected",weighted= TRUE, diag = FALSE)
eNM2 <- get.edgelist(netNM2, names=FALSE)
l2 <- qgraph.layout.fruchtermanreingold(eNM2,vcount=vcount(netNM2),area=100*(vcount(netNM2)^2),repulse.rad=(vcount(netNM2)^3.1))
E(netNM2)$color = rgb(0,0,0,.1)
V(netNM2)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netNM2, main="Visualizacion de  la estructura de la red pesada N2",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netNM1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=l2, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netNM3 <- graph.adjacency(NM3,mode="undirected",weighted= TRUE, diag = FALSE)
eNM3 <- get.edgelist(netNM3, names=FALSE)
l3 <- qgraph.layout.fruchtermanreingold(eNM3,vcount=vcount(netNM3),area=100*(vcount(netNM3)^2),repulse.rad=(vcount(netNM3)^3.1))
E(netNM3)$color = rgb(0,0,0,.1)

V(netNM3)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netNM3, main="Visualizacion de  la estructura de la red pesada N3",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netNM1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=l3, vertex.size = 10, vertex.label.cex = 0.6)

#graficar pesados
netMW <- graph.adjacency(MW,mode="undirected",weighted= TRUE, diag = FALSE)
eMW <- get.edgelist(netNM3, names=FALSE)
lW <- qgraph.layout.fruchtermanreingold(eMW,vcount=vcount(netMW),area=100*(vcount(netMW)^2),repulse.rad=(vcount(netMW)^3.1))
E(netMW)$color = rgb(0,0,0,.1)

V(netMW)$media <- aalnames #Create a vertex sequence (vs) containing all vertices of a graph.
plot.igraph(netMW, main="Visualizacion de  la estructura de la red pesada W",
            vertex.color = "yellow",vertex.frame.color = "orange", vertex.shape="sphere",
            edge.width=E(netNM1)$weight, edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1,layout=lW, vertex.size = 10, vertex.label.cex = 0.6)

## Umbral de correcion en funcion de la densidad de aristas

diag(NM1)<-0
diag(NM2)<-0
diag(NM3)<-0
diag(MW)<-0

N = dim(NM1)[1]
Nmaxlinks = N*(N-1)
out <- data.frame(matrix(ncol = 5, nrow = ((Nmaxlinks*0.15)-(round((Nmaxlinks*0.025), digits=0)))))
colnames(out)<-c("Delta", "N1", "N2", "N3", "W")

for(i in (round((Nmaxlinks*0.02), digits=0)):(Nmaxlinks*0.5)){
  n = i
  delta = n/Nmaxlinks
  tmpM1<-sort(as.vector(NM1),decreasing = TRUE)
  tmpM2<-sort(as.vector(NM2),decreasing = TRUE)
  tmpM3<-sort(as.vector(NM3),decreasing = TRUE)
  tmpMW<-sort(as.vector(MW),decreasing = TRUE)
  ro1 = tmpM1[n]
  ro2 = tmpM2[n]
  ro3 = tmpM3[n]
  roW = tmpMW[n]
  
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

ros.NM1 <- tmpM1[ns]
ros.NM2 <- tmpM2[ns]
ros.NM3 <- tmpM3[ns]
ros.MW <- tmpMW[ns]

NM1b <- data.frame(V1=logical(116))
NM2b <- data.frame(V1=logical(116))
NM3b <- data.frame(V1=logical(116))
WMb <- data.frame(V1=logical(116))

for (i in 1:length(ros.NM1)){ 
  NM1b[i] = NM1>ros.NM1[i]
}
for (i in 1:length(ros.NM2)){ 
  NM2b[i] = NM2>ros.NM2[i]
}
for (i in 1:length(ros.NM3)){ 
  NM3b[i] = NM3>ros.NM3[i]
}
for (i in 1:length(ros.MW)){ 
  WMb[i] = MW>ros.MW[i]
}

netNM1<-NM1b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netNM2<-NM2b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netNM3<-NM3b %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
netMW<-WMb %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))

plotsNM1b<-NM1b %>% map(~ ggcorrplot(.x, type = "upper", title = "N1 para δ = ", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsNM2b<-NM2b %>% map(~ ggcorrplot(.x, type = "upper", title = "N2 thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsNM3b<-NM3b %>% map(~ ggcorrplot(.x, type = "upper", title = "N3 thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))
plotsWMb<-WMb %>% map(~ ggcorrplot(.x, type = "upper", title = "W thresholded", hc.order=TRUE, show.diag=FALSE, tl.cex=5, tl.col="black", colors = brewer.pal(n = 3, name = "RdGy")))

ggarrange(plotlist=plotsNM1b)
ggarrange(plotlist=plotsNM2b)
ggarrange(plotlist=plotsNM3b)
ggarrange(plotlist=plotsWM3b)

#### numero de nodos ####
vcount.N1<-netNM1 %>% map(~ vcount(.x))
vcount.N2<-netNM2 %>% map(~ vcount(.x))
vcount.N3<-netNM3 %>% map(~ vcount(.x))
vcount.W<-netMW %>% map(~ vcount(.x))

nodos<-data.frame(deltas,unlist(vcount.N1), unlist(vcount.N2), unlist(vcount.N3), unlist(vcount.W))
colnames(nodos)<-c("Delta", "N1", "N2", "N3", "W")

ggplot(nodos, aes (x=Delta,y=N1))+
  geom_point(size=0.2, shape=21)+
  ggtitle("Umbral de corte en funcion de la densidad de nodos") +
  xlab("δ densidad de aristas") + ylab("numero de nodos")

#### numero de aristas ####
ecount.N1<-netNM1 %>% map(~ ecount(.x))
ecount.N2<-netNM2 %>% map(~ ecount(.x))
ecount.N3<-netNM3 %>% map(~ ecount(.x))
ecount.W<-netMW %>% map(~ ecount(.x))

aristas<-data.frame(deltas,unlist(ecount.N1), unlist(ecount.N2), unlist(ecount.N3), unlist(ecount.W))
colnames(aristas)<-c("Delta", "N1", "N2", "N3", "W")

ggplot(aristas, aes (x=Delta,y=N1))+
  geom_point(size=0.2, shape=21)+
  ggtitle("Umbral de corte en funcion de la densidad de aristas") +
  xlab("δ densidad de aristas") + ylab("numero de aristas")

### es un grafo simple? ######
simple.N1<-netNM1 %>% map(~ is.simple(.x))
simple.N2<-netNM2 %>% map(~ is.simple(.x))
simple.N3<-netNM3 %>% map(~ is.simple(.x))
simple.W<-netMW %>% map(~ is.simple(.x))

simples<-data.frame(deltas,unlist(simple.N1), unlist(simple.N2), unlist(simple.N3), unlist(simple.W))
colnames(simples)<-c("Delta", "N1", "N2", "N3", "W")

simp<-tableGrob(simples)
grid.newpage()
grid.draw(simp)



### es un grafo conexo? ####
connect.N1<-netNM1 %>% map(~ is.connected(.x))
connect.N2<-netNM2 %>% map(~ is.connected(.x))
connect.N3<-netNM3 %>% map(~ is.connected(.x))
connect.W<-netMW %>% map(~ is.connected(.x))

connected<-data.frame(deltas,unlist(connect.N1), unlist(connect.N2), unlist(connect.N3), unlist(connect.W))
colnames(connected)<-c("Delta", "N1", "N2", "N3", "W")

# El diametro del componente conexo
diameter.N1<-netNM1 %>% map(~ diameter(.x))
diameter.N2<-netNM2 %>% map(~ diameter(.x))
diameter.N3<-netNM3 %>% map(~ diameter(.x))
diameter.W<-netMW %>% map(~ diameter(.x))

diameters<-data.frame(deltas,unlist(diameter.N1), unlist(diameter.N2), unlist(diameter.N3), unlist(diameter.W))
colnames(diameters)<-c("Delta", "N1", "N2", "N3", "W")

ggplot(melt(diameters, id.vars = "Delta"), aes (x=Delta,y=value,color=variable))+
  geom_point(size=0.2, shape=21)+
  geom_line()+
  ggtitle("Diametro del componente conexo en funcion de δ") +
  xlab("δ densidad de aristas") + ylab("Diametro componente conexo")+
  geom_smooth()


################### CENTRALIDAD ##################

################### GRADO ##################

#Grado medio de la red
mean.degree.N1<-netNM1 %>% map(~ mean(degree(.x)))
mean.degree.N2<-netNM2 %>% map(~ mean(degree(.x)))
mean.degree.N3<-netNM3 %>% map(~ mean(degree(.x)))
mean.degree.W<-netMW %>% map(~ mean(degree(.x)))

mean.degrees<-data.frame(deltas,unlist(mean.degree.N1), unlist(mean.degree.N2), unlist(mean.degree.N3), unlist(mean.degree.W))
colnames(mean.degrees)<-c("Delta", "N1", "N2", "N3", "W")

ggplot(melt(mean.degrees, id.vars = "Delta"), aes (x=Delta,y=value,color=variable))+
  geom_point(size=0.2, shape=21)+
  geom_line()+
  ggtitle("Grado medio de la red en funcion de δ") +
  xlab("δ densidad de aristas") + ylab("Grado medio")

#Distribucion de grados
dist.degree.N1<-netNM1 %>% map(~ degree.distribution(.x, cumulative = TRUE))
dist.degree.N2<-netNM2 %>% map(~ degree.distribution(.x, cumulative = TRUE))
dist.degree.N3<-netNM3 %>% map(~ degree.distribution(.x, cumulative = TRUE))
dist.degree.W<-netMW %>% map(~ degree.distribution(.x, cumulative = TRUE))

plotdegreeN1<-netNM1 %>% map(~ plot(degree.distribution(.x, cumulative = T), type = "l", xlab = "grado", ylab = "proporción de nodos", xlim = c(0,50), ylim = c(0,1))) 

plotdegreedistN1<-netNM1 %>% map(~ plot( degree.distribution(.x),xlab = "grados", ylab = "proporción de nodos", type = "h", main = "N1", xlim = c(0,50), ylim = c(0,0.25)))

plot( degree.distribution(netNM1[[26]], cumulative = T), type = "l", xlab = "grado", ylab = "proporción denodos", xlim = c(0,50)) 
plot( degree.distribution(ws.per.red, cumulative = T), type = "l", xlab = "grado", ylab = "proporción de 9"


dd<-degree.distribution(netNM1[[1]], cumulative = TRUE)
dd<-degree.distribution(netNM1[[1]], cumulative = FALSE)

probability = dd[-1] 
nonzero.position= which(probability != 0)

probability = probability[nonzero.position]

d = degree(netNM1[[1]], mode = "all")
degree = 1:max(d)

degree = degree[nonzero.position]

reg= lm(log(probability) ~ log(degree))

summary(reg)$r.square

cozf=coef(reg)

plot(probability ~degree)

plot(probability ~ degree, log = "xy")

power.law.fit = function(x)exp(cozf[[1]]+cozf[[2]]*log(x))

curve(power.law.fit, col = "red", add =T)

g.big.ba = barabasi.game(2000)
g.big.er = erdos.renyi.game(2000, 0.1)

plot_degree_distribution(g.big.ba)


# plot and fit the power law distribution
fit_power_law = function(graph) {
  # calculate degree
  d = degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = T, n = length(d))
}


fit_power_law(g.big.ba)

################### CAMINO MINIMO ##################
dist.degree.N1<-netNM1 %>% map(~ degree.distribution(.x, cumulative = TRUE)))
head( sort( degree(netN1), decreasing = T) )
maxegreeN1<-netNM1 %>% map(~head( sort(degree(.x), decreasing = T))) 


betweennessN1<-netNM1 %>% map(~head( sort(betweenness(.x), decreasing = T))) 
betweennessN2<-netNM2 %>% map(~head( sort(betweenness(.x), decreasing = T))) 
betweennessN3<-netNM3 %>% map(~head( sort(betweenness(.x), decreasing = T))) 
betweennessW<-netMW %>% map(~head( sort(betweenness(.x), decreasing = T))) 

closenessN1<-netNM1 %>% map(~head( sort(closeness(.x), decreasing = T))) 
eigen_centralityN1<-netNM1 %>% map(~head( sort(eigen_centrality(.x)$vector, decreasing = T))) 

data.frame(betweennessN1)

head( sort( betweenness(netN1), decreasing = T))
head( sort( closeness(netN1), decreasing = T))
head( sort( eigen_centrality(netN1)$vector, decreasing = T))

################### COEFICIENTE DE CLUSTERING ##################

netN1.cl.eb <- cluster_edge_betweenness(netNM1, directed = F, merges = T)
plot(netN1, vertex.color = netN1.cl.eb$membership)


ClusteringN1<-netNM1 %>% map(~cluster_edge_betweenness(.x, directed = F, merges = T)) 
plotclustN1<-netNM1 %>% map(~ plot(.x, vertex.color = netN1.cl.eb$membership)) 

netN1.cl.eb1 <- cluster_edge_betweenness(netNM1[[1]], directed = F, merges = T)
plot.igraph(netNM1[[1]], main="Clustering N1 δ=0.025",
            vertex.color = netN1.cl.eb1$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)
plot(netNM1[[1]], vertex.color = netN1.cl.eb1$membership)

netN1.cl.eb4 <- cluster_edge_betweenness(netNM1[[4]], directed = F, merges = T)
plot(netNM1[[4]], vertex.color = netN1.cl.eb4$membership)
plot.igraph(netNM1[[4]], main="Clustering N1 δ=0.040",
            vertex.color = netN1.cl.eb4$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)


netN1.cl.eb8 <- cluster_edge_betweenness(netNM1[[8]], directed = F, merges = T)
plot(netNM1[[8]], vertex.color = netN1.cl.eb1$membership)
plot.igraph(netNM1[[8]], main="Clustering N1 δ=0.060",
            vertex.color = netN1.cl.eb8$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)


netN1.cl.eb12 <- cluster_edge_betweenness(netNM1[[12]], directed = F, merges = T)
plot(netNM1[[12]], vertex.color = netN1.cl.eb12$membership)
plot.igraph(netNM1[[12]], main="Clustering N1 δ=0.070",
            vertex.color = netN1.cl.eb12$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)


netN1.cl.eb16 <- cluster_edge_betweenness(netNM1[[16]], directed = F, merges = T)
plot(netNM1[[16]], vertex.color = netN1.cl.eb1$membership)
plot.igraph(netNM1[[16]], main="Clustering N1 δ=0.100",
            vertex.color = netN1.cl.eb16$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)


netN1.cl.eb20 <- cluster_edge_betweenness(netNM1[[20]], directed = F, merges = T)
plot(netNM1[[20]], vertex.color = netN1.cl.eb1$membership)
plot.igraph(netNM1[[20]], main="Clustering N1 δ=0.120",
            vertex.color = netN1.cl.eb20$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)



netN1.cl.eb24 <- cluster_edge_betweenness(netNM1[[24]], directed = F, merges = T)
plot(netNM1[[24]], vertex.color = netN1.cl.eb1$membership)
plot.igraph(netNM1[[24]], main="Clustering N1 δ=0.140",
            vertex.color = netN1.cl.eb24$membership,vertex.frame.color = "orange", vertex.shape="sphere",
            edge.arrow.size=0.05, edge.arrow.width=0.05, 
            vertex.label.family="Arial",vertex.label.font= 3,
            margin = -0.1, vertex.size = 10, vertex.label.cex = 0.6)

ClusteringglobalN1<-as.data.frame(netNM1 %>% map(~transitivity(.x, type = "global")))
ClusteringglobalN2<-as.data.frame(netNM2 %>% map(~transitivity(.x, type = "global")))
ClusteringglobalN3<-as.data.frame(netNM3 %>% map(~transitivity(.x, type = "global")))
ClusteringglobalW<-as.data.frame(netMW %>% map(~transitivity(.x, type = "global")))

Clusteringglobalall<-rbind(ClusteringglobalN1,ClusteringglobalN2,ClusteringglobalN3,ClusteringglobalW)
rownames(Clusteringglobalall)<-c("N1", "N2", "N3", "W")

Clusteringglobalall<-as.data.frame(t(Clusteringglobalall))
Clusteringglobalall$DELTA<-deltas

transitivity(ws.per.red, type = "global")


pd <- position_dodge(0.01)

ggplot(melt(Clusteringglobalall, id.vars = "DELTA"), aes (x=DELTA,y=value,color=variable))+
  geom_point(size=0.2, shape=21)+
  geom_line()+
  ggtitle("Coeficiente de Clustering Global") +
  xlab("δ densidad de aristas") + ylab("Coefs. de clustering")+
  geom_smooth()


ClusteringlocalN1<-as.data.frame(netNM1 %>% map(~transitivity(.x, type = "local")))
ClusteringlocalN2<-as.data.frame(netNM2 %>% map(~transitivity(.x, type = "local")))
ClusteringlocalN3<-as.data.frame(netNM3 %>% map(~transitivity(.x, type = "local")))
ClusteringlocalW<-as.data.frame(netMW %>% map(~transitivity(.x, type = "local")))

clusmeanN1<-colMeans(!is.na(ClusteringlocalN1))
clusmeanN2<-colMeans(!is.na(ClusteringlocalN2))
clusmeanN3<-colMeans(!is.na(ClusteringlocalN3))
clusmeanW<-colMeans(!is.na(ClusteringlocalW))
Clustlocalmeantotal<-rbind(clusmeanN1,clusmeanN2,clusmeanN3,clusmeanW)

Clustlocalmeantotal<-as.data.frame(t(Clustlocalmeantotal))
Clustlocalmeantotal$DELTA<-deltas

ggplot(melt(Clustlocalmeantotal, id.vars = "DELTA"), aes (x=DELTA,y=value,color=variable))+
  geom_point(size=0.2, shape=21)+
  geom_line()+
  ggtitle("Valor Medio del Coeficiente de Clustering Local") +
  xlab("δ densidad de aristas") + ylab("Coefs. de clustering")+
  geom_smooth()


mean(ClusteringlocalN1[,1])
