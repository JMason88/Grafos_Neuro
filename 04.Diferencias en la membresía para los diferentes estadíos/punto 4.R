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
library(data.table)
library(Rmisc) 
library(tensr)
library(permute)
setwd("/Volumes/Macintosh HD/Users/Mario/Dropbox/Maestria Data Minin/DM CyT/TP2/DataSujetos+AAL/DataSujetos")
setwd("E:/MARIO/Dropbox/Maestria Data Minin/DM CyT/TP2/DataSujetos+AAL/DataSujetos")

#### crea una lista de files ####
N1s_suj<-list.files( pattern="(N1).*\\.csv$")
N2s_suj<-list.files( pattern="(N2).*\\.csv$")
N3s_suj<-list.files( pattern="(N3).*\\.csv$")
Ws_suj<-list.files( pattern="(W).*\\.csv$")
#### lee la lista de files ####
N1_sujs<-map(N1s_suj,read.csv, header=FALSE)
N2_sujs<-map(N2s_suj,read.csv, header=FALSE)
N3_sujs<-map(N3s_suj,read.csv, header=FALSE)
W_sujs<-map(Ws_suj,read.csv, header=FALSE)
#### extrae nombre de regiones cerebros####
aal <- read.csv("aal_extended.csv", header = F)
aalnames <- as.character(aal[,2])
#### agrega nombre de columnas ####
N1sujs.C<-N1_sujs %>%map(~.x %>% set_colnames(aalnames))
N2sujs.C<-N2_sujs %>%map(~.x %>% set_colnames(aalnames))
N3sujs.C<-N3_sujs %>%map(~.x %>% set_colnames(aalnames))
Wsujs.C<-W_sujs %>%map(~.x %>% set_colnames(aalnames))
#### agrega nombre de filas ####
N1sujs.R<-N1sujs.C %>%map(~.x %>% set_rownames(aalnames))
N2sujs.R<-N2sujs.C %>%map(~.x %>% set_rownames(aalnames))
N3sujs.R<-N3sujs.C %>%map(~.x %>% set_rownames(aalnames))
Wsujs.R<-Wsujs.C %>%map(~.x %>% set_rownames(aalnames))
#### Crea la matriz #####
N1s <- N1sujs.R %>% map(~ as.matrix(.x))
N2s <- N2sujs.R %>% map(~ as.matrix(.x))
N3s <- N3sujs.R %>% map(~ as.matrix(.x))
Ws <- Wsujs.R %>% map(~ as.matrix(.x))
#### Defino Nmaxlinks #####
N = dim(N1s[[1]])[1]
Nmaxlinks = N*(N-1)
#### Defino intervalo de densidades #####
deltas<-seq(from=0.02, to=0.52, by=0.02)
#### indices para los culaes tengo esas densidades #####
ns<-deltas*Nmaxlinks
#### todas las diagonales a 0 #####
for (i in 1:18) {
  diag(N1s[[i]])<-0
}
for (i in 1:18) {
  diag(N2s[[i]])<-0
}
for (i in 1:18) {
  diag(N3s[[i]])<-0
}
for (i in 1:18) {
  diag(Ws[[i]])<-0
}
#### creo un vector ordenado con todos los valores de correlaciones #####
tmp1s<-N1s%>%map(~sort(as.vector(.x),decreasing = TRUE))
tmp2s<-N2s%>%map(~sort(as.vector(.x),decreasing = TRUE))
tmp3s<-N3s%>%map(~sort(as.vector(.x),decreasing = TRUE))
tmpWs<-Ws%>%map(~sort(as.vector(.x),decreasing = TRUE))
#### calculo los Ros de cada Delta para cada individuo #####
ros.N1s <- 1:18%>%map(~tmp1s[[.x]][ns])
ros.N2s <- 1:18%>%map(~tmp2s[[.x]][ns])
ros.N3s <- 1:18%>%map(~tmp3s[[.x]][ns])
ros.Ws <- 1:18%>%map(~tmpWs[[.x]][ns])
#### calculo los Ros de cada Delta para cada individuo #####
N1bs <- vector("list", 18) 
for (j in 1:18) {
N1bs[[j]] <- data.frame(V1=logical(116))
for (i in 1:length(ros.N1s[[j]])){ 
  N1bs[[j]][i] = N1s[[j]]>ros.N1s[[j]][i]
}
}
N2bs <- vector("list", 18) 
for (j in 1:18) {
  N2bs[[j]] <- data.frame(V1=logical(116))
  for (i in 1:length(ros.N2s[[j]])){ 
    N2bs[[j]][i] = N2s[[j]]>ros.N2s[[j]][i]
  }
}
N3bs <- vector("list", 18) 
for (j in 1:18) {
  N3bs[[j]] <- data.frame(V1=logical(116))
  for (i in 1:length(ros.N3s[[j]])){ 
    N3bs[[j]][i] = N3s[[j]]>ros.N3s[[j]][i]
  }
}
Wbs <- vector("list", 18) 
for (j in 1:18) {
  Wbs[[j]] <- data.frame(V1=logical(116))
  for (i in 1:length(ros.Ws[[j]])){ 
    Wbs[[j]][i] = Ws[[j]]>ros.Ws[[j]][i]
  }
}
#### Construyo las matrices de adyacencia  para Ro de cada Delta para cada individuo ####
netN1s <- vector("list", 18) 
for (i in 1:18) {
netN1s[[i]]<-N1bs[[i]] %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
}
netN2s <- vector("list", 18) 
for (i in 1:18) {
  netN2s[[i]]<-N2bs[[i]] %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
}
netN3s <- vector("list", 18) 
for (i in 1:18) {
  netN3s[[i]]<-N3bs[[i]] %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
}
netWs <- vector("list", 18) 
for (i in 1:18) {
  netWs[[i]]<-Wbs[[i]] %>% map(~ graph.adjacency(.x,mode="undirected",diag = FALSE))
}
#### Claculo clustering de Louvain para cada matriz de adyacencia para cada Ro de cada Delta para cada individuo ####
netN1s.cl.lo <- vector("list", 18) 
for (j in 1:18) {
  for (i in 1:26){ 
    netN1s.cl.lo[[j]][i]<-netN1s[[j]][i]%>%map(~cluster_louvain(.x))
  }
}

netN2s.cl.lo <- vector("list", 18) 
for (j in 1:18) {
  for (i in 1:26){ 
    netN2s.cl.lo[[j]][i]<-netN2s[[j]][i]%>%map(~cluster_louvain(.x))
  }
}

netN3s.cl.lo <- vector("list", 18) 
for (j in 1:18) {
  for (i in 1:26){ 
    netN3s.cl.lo[[j]][i]<-netN3s[[j]][i]%>%map(~cluster_louvain(.x))
  }
}

netWs.cl.lo <- vector("list", 18) 
for (j in 1:18) {
  for (i in 1:26){ 
    netWs.cl.lo[[j]][i]<-netWs[[j]][i]%>%map(~cluster_louvain(.x))
  }
}

####calculo rand index####
rWN1<-matrix(nrow = 18, ncol = 26)
for (j in 1:18) {
  for (i in 1:26) {
    rWN1[j,i]<- (compare(netWs.cl.lo[[j]][[i]],netN1s.cl.lo[[j]][[i]], method = "adjusted.rand"))
  }
}

colnames(rWN1)<-deltas
rownames(rWN1)<-c(1:18)



rWN2<-matrix(nrow = 18, ncol = 26)
for (j in 1:18) {
  for (i in 1:26) {
    rWN2[j,i]<- (compare(netWs.cl.lo[[j]][[i]],netN2s.cl.lo[[j]][[i]], method = "adjusted.rand"))
  }
}

colnames(rWN2)<-deltas
rownames(rWN2)<-c(1:18)


rWN3<-matrix(nrow = 18, ncol = 26)
for (j in 1:18) {
  for (i in 1:26) {
    rWN3[j,i]<- (compare(netWs.cl.lo[[j]][[i]],netN3s.cl.lo[[j]][[i]], method = "adjusted.rand"))
  }
}

colnames(rWN3)<-deltas
rownames(rWN3)<-c(1:18)


####preparo para graficar###

rWN1.M<-melt(rWN1)
rWN1.M$Comp<-"Wake_vs_N1"
rWN2.M<-melt(rWN2)
rWN2.M$Comp<-"Wake_vs_N2"
rWN3.M<-melt(rWN3)
rWN3.M$Comp<-"Wake_vs_N3"

rWNX<-rbind(rWN1.M,rWN2.M,rWN3.M)
rWNX$Tipo<-"Control"
colnames(rWNX)<-c("Indiv","Delta","RI", "Comp", "Tipo")

### Grafico ####
RWNX <- summarySE(rWNX, measurevar="RI", groupvars=c("Delta", "Comp"))
RWNX$Tipo<-"Control"

# Standard error of the mean
pd <- position_dodge(0.01)
ggplot(RWNX, aes(x=Delta, y=RI, colour=Comp)) + 
  geom_errorbar(aes(ymin=RI-se, ymax=RI+se), width=.003,colour="black", position=pd) +
  geom_line() +
  geom_point(position=pd, size=3, shape=21, fill="white")+
xlab("δ densidad de aristas") +
  ylab("ARI (Indice de Rand Ajustado Grupos Originales") +
  ggtitle("Indice de Rand en funcion de δ") +
  expand_limits(y=0.15) +
  scale_y_continuous(breaks=seq(from=0.1, to=0.5, by=0.05))+
  geom_smooth()

########## Randomizo para cada estadio######
####calculo rand index####

# Defino una red con las propiedades de la red percibida.
tmpN1.comm <- netN1s.cl.lo
tmpN2.comm <- netN2s.cl.lo
tmpN3.comm <- netN3s.cl.lo

# Asigno etiquetas al azar y vuelvo a calcular
dims <- c(18,26,1000)

randomN1.rand <- array(0,dims)
for (j in 1:18) {
  for (i in 1:26) {
    for(h in 1:1000) {
      tmpN1.comm[[j]][[i]]$membership <- as.numeric(sample(1:(max(unique(tmpN1.comm[[j]][[i]]$membership))), 116, replace = T))
      randomN1.rand[j,i,h] <- compare(netWs.cl.lo[[j]][[i]], tmpN1.comm[[j]][[i]], method = "adjusted.rand" )
    }
  }
}



randomN2.rand <- array(0,dims)
for (j in 1:18) {
  for (i in 1:26) {
    for(h in 1:1000) {
      tmpN2.comm[[j]][[i]]$membership <- as.numeric(sample(1:(max(unique(tmpN2.comm[[j]][[i]]$membership))), 116, replace = T))
      randomN2.rand[j,i,h] <- compare(netWs.cl.lo[[j]][[i]], tmpN2.comm[[j]][[i]], method = "adjusted.rand" )
    }
  }
}

randomN3.rand <- array(0,dims)
for (j in 1:18) {
  for (i in 1:26) {
    for(h in 1:1000) {
      tmpN3.comm[[j]][[i]]$membership <- as.numeric(sample(1:(max(unique(tmpN3.comm[[j]][[i]]$membership))), 116, replace = T))
      randomN3.rand[j,i,h] <- compare(netWs.cl.lo[[j]][[i]], tmpN3.comm[[j]][[i]], method = "adjusted.rand" )
    }
  }
}



randomN1.rand.M<-as.data.frame(mat(randomN1.rand,3))

eN11<-select(randomN1.rand.M,V1:V18)
colnames(eN11)<-1:18
eN12<-select(randomN1.rand.M,V19:V36)
colnames(eN12)<-1:18
eN13<-select(randomN1.rand.M,V37:V54)
colnames(eN13)<-1:18
eN14<-select(randomN1.rand.M,V55:V72)
colnames(eN14)<-1:18
eN15<-select(randomN1.rand.M,V73:V90)
colnames(eN15)<-1:18
eN16<-select(randomN1.rand.M,V91:V108)
colnames(eN16)<-1:18
eN17<-select(randomN1.rand.M,V109:V126)
colnames(eN17)<-1:18
eN18<-select(randomN1.rand.M,V127:V144)
colnames(eN18)<-1:18
eN19<-select(randomN1.rand.M,V145:V162)
colnames(eN19)<-1:18
eN110<-select(randomN1.rand.M,V163:V180)
colnames(eN110)<-1:18
eN111<-select(randomN1.rand.M,V181:V198)
colnames(eN111)<-1:18
eN112<-select(randomN1.rand.M,V199:V216)
colnames(eN112)<-1:18
eN113<-select(randomN1.rand.M,V217:V234)
colnames(eN113)<-1:18
eN114<-select(randomN1.rand.M,V235:V252)
colnames(eN114)<-1:18
eN115<-select(randomN1.rand.M,V253:V270)
colnames(eN115)<-1:18
eN116<-select(randomN1.rand.M,V271:V288)
colnames(eN116)<-1:18
eN117<-select(randomN1.rand.M,V289:V306)
colnames(eN117)<-1:18
eN118<-select(randomN1.rand.M,V307:V324)
colnames(eN118)<-1:18
eN119<-select(randomN1.rand.M,V325:V342)
colnames(eN119)<-1:18
eN120<-select(randomN1.rand.M,V343:V360)
colnames(eN120)<-1:18
eN121<-select(randomN1.rand.M,V361:V378)
colnames(eN121)<-1:18
eN122<-select(randomN1.rand.M,V379:V396)
colnames(eN122)<-1:18
eN123<-select(randomN1.rand.M,V397:V414)
colnames(eN123)<-1:18
eN124<-select(randomN1.rand.M,V415:V432)
colnames(eN124)<-1:18
eN125<-select(randomN1.rand.M,V433:V450)
colnames(eN125)<-1:18
eN126<-select(randomN1.rand.M,V451:V468)
colnames(eN126)<-1:18

eeN1<-rbind(eN11,eN12,eN13,eN14,eN15,eN16,eN17,eN18,eN19,eN110,eN111,eN112,eN113,eN114,eN115,eN116,eN117,eN118,eN119,eN120,eN121,eN122,eN123,eN124,eN125,eN126)

eeN1$Delta<-rep(seq(from=0.02, to=0.52, by=0.02), each=1000)
eeN1m<-melt(eeN1, id.vars = "Delta")

RRWN1 <- summarySE(eeN1m, measurevar="value", groupvars=c("Delta"))

randomN2.rand.M<-as.data.frame(mat(randomN2.rand,3))

eN21<-select(randomN2.rand.M,V1:V18)
colnames(eN21)<-1:18
eN22<-select(randomN2.rand.M,V19:V36)
colnames(eN22)<-1:18
eN23<-select(randomN2.rand.M,V37:V54)
colnames(eN23)<-1:18
eN24<-select(randomN2.rand.M,V55:V72)
colnames(eN24)<-1:18
eN25<-select(randomN2.rand.M,V73:V90)
colnames(eN25)<-1:18
eN26<-select(randomN2.rand.M,V91:V108)
colnames(eN26)<-1:18
eN27<-select(randomN2.rand.M,V109:V126)
colnames(eN27)<-1:18
eN28<-select(randomN2.rand.M,V127:V144)
colnames(eN28)<-1:18
eN29<-select(randomN2.rand.M,V145:V162)
colnames(eN29)<-1:18
eN210<-select(randomN2.rand.M,V163:V180)
colnames(eN210)<-1:18
eN211<-select(randomN2.rand.M,V181:V198)
colnames(eN211)<-1:18
eN212<-select(randomN2.rand.M,V199:V216)
colnames(eN212)<-1:18
eN213<-select(randomN2.rand.M,V217:V234)
colnames(eN213)<-1:18
eN214<-select(randomN2.rand.M,V235:V252)
colnames(eN214)<-1:18
eN215<-select(randomN2.rand.M,V253:V270)
colnames(eN215)<-1:18
eN216<-select(randomN2.rand.M,V271:V288)
colnames(eN216)<-1:18
eN217<-select(randomN2.rand.M,V289:V306)
colnames(eN217)<-1:18
eN218<-select(randomN2.rand.M,V307:V324)
colnames(eN218)<-1:18
eN219<-select(randomN2.rand.M,V325:V342)
colnames(eN219)<-1:18
eN220<-select(randomN2.rand.M,V343:V360)
colnames(eN220)<-1:18
eN221<-select(randomN2.rand.M,V361:V378)
colnames(eN221)<-1:18
eN222<-select(randomN2.rand.M,V379:V396)
colnames(eN222)<-1:18
eN223<-select(randomN2.rand.M,V397:V414)
colnames(eN223)<-1:18
eN224<-select(randomN2.rand.M,V415:V432)
colnames(eN224)<-1:18
eN225<-select(randomN2.rand.M,V433:V450)
colnames(eN225)<-1:18
eN226<-select(randomN2.rand.M,V451:V468)
colnames(eN226)<-1:18

eeN2<-rbind(eN21,eN22,eN23,eN24,eN25,eN26,eN27,eN28,eN29,eN210,eN211,eN212,eN213,eN214,eN215,eN216,eN217,eN218,eN219,eN220,eN221,eN222,eN223,eN224,eN225,eN226)

eeN2$Delta<-rep(seq(from=0.02, to=0.52, by=0.02), each=1000)
eeN2m<-melt(eeN2, id.vars = "Delta")

RRWN2 <- summarySE(eeN2m, measurevar="value", groupvars=c("Delta"))

randomN3.rand.M<-as.data.frame(mat(randomN3.rand,3))

eN31<-select(randomN3.rand.M,V1:V18)
colnames(eN31)<-1:18
eN32<-select(randomN3.rand.M,V19:V36)
colnames(eN32)<-1:18
eN33<-select(randomN3.rand.M,V37:V54)
colnames(eN33)<-1:18
eN34<-select(randomN3.rand.M,V55:V72)
colnames(eN34)<-1:18
eN35<-select(randomN3.rand.M,V73:V90)
colnames(eN35)<-1:18
eN36<-select(randomN3.rand.M,V91:V108)
colnames(eN36)<-1:18
eN37<-select(randomN3.rand.M,V109:V126)
colnames(eN37)<-1:18
eN38<-select(randomN3.rand.M,V127:V144)
colnames(eN38)<-1:18
eN39<-select(randomN3.rand.M,V145:V162)
colnames(eN39)<-1:18
eN310<-select(randomN3.rand.M,V163:V180)
colnames(eN310)<-1:18
eN311<-select(randomN3.rand.M,V181:V198)
colnames(eN311)<-1:18
eN312<-select(randomN3.rand.M,V199:V216)
colnames(eN312)<-1:18
eN313<-select(randomN3.rand.M,V217:V234)
colnames(eN313)<-1:18
eN314<-select(randomN3.rand.M,V235:V252)
colnames(eN314)<-1:18
eN315<-select(randomN3.rand.M,V253:V270)
colnames(eN315)<-1:18
eN316<-select(randomN3.rand.M,V271:V288)
colnames(eN316)<-1:18
eN317<-select(randomN3.rand.M,V289:V306)
colnames(eN317)<-1:18
eN318<-select(randomN3.rand.M,V307:V324)
colnames(eN318)<-1:18
eN319<-select(randomN3.rand.M,V325:V342)
colnames(eN319)<-1:18
eN320<-select(randomN3.rand.M,V343:V360)
colnames(eN320)<-1:18
eN321<-select(randomN3.rand.M,V361:V378)
colnames(eN321)<-1:18
eN322<-select(randomN3.rand.M,V379:V396)
colnames(eN322)<-1:18
eN323<-select(randomN3.rand.M,V397:V414)
colnames(eN323)<-1:18
eN324<-select(randomN3.rand.M,V415:V432)
colnames(eN324)<-1:18
eN325<-select(randomN3.rand.M,V433:V450)
colnames(eN325)<-1:18
eN326<-select(randomN3.rand.M,V451:V468)
colnames(eN326)<-1:18

eeN3<-rbind(eN31,eN32,eN33,eN34,eN35,eN36,eN37,eN38,eN39,eN310,eN311,eN312,eN313,eN314,eN315,eN316,eN317,eN318,eN319,eN320,eN321,eN322,eN323,eN324,eN325,eN326)

eeN3$Delta<-rep(seq(from=0.02, to=0.52, by=0.02), each=1000)
eeN3m<-melt(eeN3, id.vars = "Delta")

RRWN3 <- summarySE(eeN3m, measurevar="value", groupvars=c("Delta"))



eeN1m$Comp<-"Wake_vs_N1"
eeN2m$Comp<-"Wake_vs_N2"
eeN3m$Comp<-"Wake_vs_N3"

rRWNX<-rbind(eeN1m,eeN2m,eeN3m)
rRWNX$Tipo<-"Randomizados"
colnames(rRWNX)<-c("Delta","Indiv","RI", "Comp", "Tipo")

RRWNX <- summarySE(rRWNX, measurevar="RI", groupvars=c("Delta", "Comp"))
RRWNX$Tipo<-"Randomizados"

pd <- position_dodge(0.01)
  ggplot(RRWNX, aes(x=Delta, y=RI, colour=Comp)) + 
  geom_errorbar(aes(ymin=RI-se, ymax=RI+se), width=.003,colour="black", position=pd) +
  geom_line() +
  geom_point(position=pd, size=3, shape=21, fill="white")+
  xlab("δ densidad de aristas") +
  ylab("ARI (Iindice de Rand Ajustado") +
  ggtitle("Indice de Rand en funcion de δ Grupos Randomizados") +
  expand_limits(y=0.15) +
  scale_y_continuous(breaks=seq(from=0.1, to=0.5, by=0.05))+
  geom_smooth()



  
RR.merge.WNX<-rbind(RRWNX,RWNX)

ggplot(RR.merge.WNX, aes(x=Delta, y=RI, colour=Tipo)) + 
  geom_errorbar(aes(ymin=RI-se, ymax=RI+se), width=.003,colour="black") +
  geom_line() +
  geom_point(size=3, shape=21, fill="white")+
  xlab("δ densidad de aristas") +
  ylab("ARI (Iindice de Rand Ajustado") +
  ggtitle("Indice de Rand en funcion de δ Grupos Randomizados") +
  geom_smooth()+
  facet_grid(. ~ Comp)


table( eeN1[1:1000,-19] < RWNX [1,4])/18000
table( eeN1[1001:2000,-19] < RWNX [4,4])/18000
table( eeN1[2001:3000,-19] < RWNX [7,4])/18000
table( eeN1[3001:4000,-19] < RWNX [10,4])/18000
table( eeN1[4001:5000,-19] < RWNX [13,4])/18000
table( eeN1[5001:6000,-19] < RWNX [16,4])/18000
table( eeN1[6001:7000,-19] < RWNX [19,4])/18000
table( eeN1[7001:8000,-19] < RWNX [22,4])/18000
table( eeN1[8001:9000,-19] < RWNX [25,4])/18000
table( eeN1[9001:10000,-19] < RWNX [28,4])/18000
table( eeN1[10001:11000,-19] < RWNX [31,4])/18000
table( eeN1[11001:12000,-19] < RWNX [34,4])/18000
table( eeN1[12001:13000,-19] < RWNX [37,4])/18000
table( eeN1[13001:14000,-19] < RWNX [40,4])/18000
table( eeN1[14001:15000,-19] < RWNX [43,4])/18000
table( eeN1[15001:16000,-19] < RWNX [46,4])/18000
table( eeN1[16001:17000,-19] < RWNX [49,4])/18000
table( eeN1[17001:18000,-19] < RWNX [52,4])/18000
table( eeN1[18001:19000,-19] < RWNX [55,4])/18000
table( eeN1[19001:20000,-19] < RWNX [58,4])/18000
table( eeN1[20001:21000,-19] < RWNX [61,4])/18000
table( eeN1[21001:22000,-19] < RWNX [64,4])/18000
table( eeN1[22001:23000,-19] < RWNX [67,4])/18000
table( eeN1[23001:24000,-19] < RWNX [70,4])/18000
table( eeN1[24001:25000,-19] < RWNX [73,4])/18000
table( eeN1[25001:26000,-19] < RWNX [76,4])/18000

table( eeN2[1:1000,-19] < RWNX [2,4])/18000
table( eeN2[1001:2000,-19] < RWNX [5,4])/18000
table( eeN2[2001:3000,-19] < RWNX [8,4])/18000
table( eeN2[3001:4000,-19] < RWNX [11,4])/18000
table( eeN2[4001:5000,-19] < RWNX [14,4])/18000
table( eeN2[5001:6000,-19] < RWNX [17,4])/18000
table( eeN2[6001:7000,-19] < RWNX [20,4])/18000
table( eeN2[7001:8000,-19] < RWNX [23,4])/18000
table( eeN2[8001:9000,-19] < RWNX [26,4])/18000
table( eeN2[9001:10000,-19] < RWNX [29,4])/18000
table( eeN2[10001:11000,-19] < RWNX [32,4])/18000
table( eeN2[11001:12000,-19] < RWNX [35,4])/18000
table( eeN2[12001:13000,-19] < RWNX [38,4])/18000
table( eeN2[13001:14000,-19] < RWNX [41,4])/18000
table( eeN2[14001:15000,-19] < RWNX [44,4])/18000
table( eeN2[15001:16000,-19] < RWNX [47,4])/18000
table( eeN2[16001:17000,-19] < RWNX [50,4])/18000
table( eeN2[17001:18000,-19] < RWNX [53,4])/18000
table( eeN2[18001:19000,-19] < RWNX [56,4])/18000
table( eeN2[19001:20000,-19] < RWNX [59,4])/18000
table( eeN2[20001:21000,-19] < RWNX [62,4])/18000
table( eeN2[21001:22000,-19] < RWNX [65,4])/18000
table( eeN2[22001:23000,-19] < RWNX [68,4])/18000
table( eeN2[23001:24000,-19] < RWNX [71,4])/18000
table( eeN2[24001:25000,-19] < RWNX [74,4])/18000
table( eeN2[25001:26000,-19] < RWNX [77,4])/18000

table( eeN3[1:1000,-19] < RWNX [3,4])/18000
table( eeN3[1001:2000,-19] < RWNX [6,4])/18000
table( eeN3[2001:3000,-19] < RWNX [9,4])/18000
table( eeN3[3001:4000,-19] < RWNX [12,4])/18000
table( eeN3[4001:5000,-19] < RWNX [15,4])/18000
table( eeN3[5001:6000,-19] < RWNX [18,4])/18000
table( eeN3[6001:7000,-19] < RWNX [21,4])/18000
table( eeN3[7001:8000,-19] < RWNX [24,4])/18000
table( eeN3[8001:9000,-19] < RWNX [27,4])/18000
table( eeN3[9001:10000,-19] < RWNX [30,4])/18000
table( eeN3[10001:11000,-19] < RWNX [33,4])/18000
table( eeN3[11001:12000,-19] < RWNX [36,4])/18000
table( eeN3[12001:13000,-19] < RWNX [39,4])/18000
table( eeN3[13001:14000,-19] < RWNX [42,4])/18000
table( eeN3[14001:15000,-19] < RWNX [45,4])/18000
table( eeN3[15001:16000,-19] < RWNX [48,4])/18000
table( eeN3[16001:17000,-19] < RWNX [51,4])/18000
table( eeN3[17001:18000,-19] < RWNX [54,4])/18000
table( eeN3[18001:19000,-19] < RWNX [57,4])/18000
table( eeN3[19001:20000,-19] < RWNX [60,4])/18000
table( eeN3[20001:21000,-19] < RWNX [63,4])/18000
table( eeN3[21001:22000,-19] < RWNX [66,4])/18000
table( eeN3[22001:23000,-19] < RWNX [69,4])/18000
table( eeN3[23001:24000,-19] < RWNX [72,4])/18000
table( eeN3[24001:25000,-19] < RWNX [75,4])/18000
table( eeN3[25001:26000,-19] < RWNX [78,4])/18000

