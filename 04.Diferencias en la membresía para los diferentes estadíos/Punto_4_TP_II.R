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

# setwd("/Volumes/Macintosh HD/Users/Mario/Dropbox/Maestria Data Minin/DM CyT/TP2/DataSujetos+AAL/DataSujetos")
setwd("C:/Users/Fabio/Documents/Maestria/CienciaYTech/tp2/github_clone/DataSujetos/")


N1_suj <- list.files(pattern = "(N1).*\\.csv$")
N2_suj <- list.files(pattern = "(N2).*\\.csv$")
N3_suj <- list.files(pattern = "(N3).*\\.csv$")
W_suj <- list.files(pattern = "(W).*\\.csv$")


N1_sujs <- map(N1_suj, read.csv, header = FALSE)
N2_sujs <- map(N2_suj, read.csv, header = FALSE)
N3_sujs <- map(N3_suj, read.csv, header = FALSE)
W_sujs <- map(W_suj, read.csv, header = FALSE)

N1promed <- Reduce("+", N1_sujs) / length(N1_sujs)
N2promed <- Reduce("+", N2_sujs) / length(N1_sujs)
N3promed <- Reduce("+", N3_sujs) / length(N1_sujs)
Wpromed <- Reduce("+", W_sujs) / length(N1_sujs)

setwd("C:/Users/Fabio/Documents/Maestria/CienciaYTech/tp2/github_clone/")

write.csv(N1promed, file = "N1promed.csv")
write.csv(N2promed, file = "N2promed.csv")
write.csv(N3promed, file = "N3promed.csv")
write.csv(Wpromed, file = "Wpromed.csv")

humbral = 0.55

N1promed.humb = ifelse(as.matrix(N1promed) >= humbral, 1, 0)
N2promed.humb = ifelse(as.matrix(N2promed) >= humbral, 1, 0)
N3promed.humb = ifelse(as.matrix(N3promed) >= humbral, 1, 0)
Wpromed.humb = ifelse(as.matrix(Wpromed) >= humbral, 1, 0)

N1promed.red <- graph.adjacency(as.matrix(N1promed.humb), mode = "undirected", diag = FALSE, weighted = T)
N2promed.red <- graph.adjacency(as.matrix(N2promed.humb), mode = "undirected", diag = FALSE, weighted = T)
N3promed.red <- graph.adjacency(as.matrix(N3promed.humb), mode = "undirected", diag = FALSE, weighted = T)
Wpromed.red <- graph.adjacency(as.matrix(Wpromed.humb), mode = "undirected", diag = FALSE, weighted = T)

N1prom.clus.betw <- cluster_edge_betweenness(N1promed.red, directed = F, merges = T)
N1prom.clus.louv <- cluster_louvain(N1promed.red, weights = E(N1promed.red)$weight)
N2prom.clus.betw <- cluster_edge_betweenness(N2promed.red, directed = F, merges = T)
N2prom.clus.louv <- cluster_louvain(N2promed.red, weights = E(N2promed.red)$weight)
N3prom.clus.betw <- cluster_edge_betweenness(N3promed.red, directed = F, merges = T)
N3prom.clus.louv <- cluster_louvain(N3promed.red, weights = E(N3promed.red)$weight)
Wprom.clus.betw <- cluster_edge_betweenness(Wpromed.red, directed = F, merges = T)
Wprom.clus.louv <- cluster_louvain(Wpromed.red, weights = E(Wpromed.red)$weight)

plot(N1promed.red, vertex.color = N1prom.clus.betw$membership, main = "N1 betweennes")
plot(N1promed.red, vertex.color = N1prom.clus.louv$membership, main = "N1 Louvain")
plot(N2promed.red, vertex.color = N2prom.clus.betw$membership, main = "N2 betweennes")
plot(N@promed.red, vertex.color = N2prom.clus.louv$membership, main = "N2 Louvain")
plot(N3promed.red, vertex.color = N3prom.clus.betw$membership, main = "N3 betweennes")
plot(N3promed.red, vertex.color = N3prom.clus.louv$membership, main = "N3 Louvain")
plot(Wpromed.red, vertex.color = Wprom.clus.betw$membership, main = "W betweennes")
plot(Wpromed.red, vertex.color = Wprom.clus.louv$membership, main = "W Louvain")

modularity(N1promed.red, N1prom.clus.betw$membership)
modularity(N1promed.red, N1prom.clus.louv$membership)
modularity(N2promed.red, N2prom.clus.betw$membership)
modularity(N2promed.red, N2prom.clus.louv$membership)
modularity(N3promed.red, N3prom.clus.betw$membership)
modularity(N3promed.red, N3prom.clus.louv$membership)
modularity(Wpromed.red, Wprom.clus.betw$membership)
modularity(Wpromed.red, Wprom.clus.louv$membership)

random.membership.N1.betw <- array()
random.membership.N2.betw <- array()
random.membership.N3.betw <- array()
random.membership.W.betw <- array()

random.membership.N1.louv <- array()
random.membership.N2.louv <- array()
random.membership.N3.louv <- array()
random.membership.W.louv <- array()

groups.N1.betw = length(table(N1prom.clus.betw$membership))
groups.N1.louv = length(table(N1prom.clus.louv$membership))
groups.N2.betw = length(table(N2prom.clus.betw$membership))
groups.N2.louv = length(table(N2prom.clus.louv$membership))
groups.N3.betw = length(table(N3prom.clus.betw$membership))
groups.N3.louv = length(table(N3prom.clus.louv$membership))
groups.W.betw = length(table(Wprom.clus.betw$membership))
groups.W.louv = length(table(Wprom.clus.louv$membership))


ln = length(N1promed)
for (i in 1:1000) random.membership.N1.louv[i] <- modularity(N1promed.red, sample(1:groups.N1.louv, ln, replace = T))
table(modularity(N1promed.red, N1prom.clus.louv$membership) > random.membership.N1.louv)
for (i in 1:1000) random.membership.N1.betw[i] <- modularity(N1promed.red, sample(1:groups.N1.betw, ln, replace = T))
table(modularity(N1promed.red, N1prom.clus.betw$membership) > random.membership.N1.betw)

for (i in 1:1000) random.membership.N2.louv[i] <- modularity(N2promed.red, sample(1:groups.N2.louv, ln, replace = T))
table(modularity(N2promed.red, N2prom.clus.louv$membership) > random.membership.N2.louv)
for (i in 1:1000) random.membership.N2.betw[i] <- modularity(N2promed.red, sample(1:groups.N2.betw, ln, replace = T))
table(modularity(N2promed.red, N2prom.clus.betw$membership) > random.membership.N2.betw)

for (i in 1:1000) random.membership.N3.louv[i] <- modularity(N3promed.red, sample(1:groups.N3.louv, ln, replace = T))
table(modularity(N3promed.red, N3prom.clus.louv$membership) > random.membership.N3.louv)
for (i in 1:1000) random.membership.N3.betw[i] <- modularity(N3promed.red, sample(1:groups.N3.betw, ln, replace = T))
table(modularity(N3promed.red, N3prom.clus.betw$membership) > random.membership.N3.betw)

for (i in 1:1000) random.membership.W.louv[i] <- modularity(Wpromed.red, sample(1:groups.W.louv, ln, replace = T))
table(modularity(Wpromed.red, Wprom.clus.louv$membership) > random.membership.W.louv)
for (i in 1:1000) random.membership.W.betw[i] <- modularity(Wpromed.red, sample(1:groups.W.betw, ln, replace = T))
table(modularity(Wpromed.red, Wprom.clus.betw$membership) > random.membership.W.betw)


compare(N1prom.clus.louv, Wprom.clus.louv, method = "adjusted.rand")
compare(N2prom.clus.louv, Wprom.clus.louv, method = "adjusted.rand")
compare(N3prom.clus.louv, Wprom.clus.louv, method = "adjusted.rand")

compare(N1prom.clus.betw, Wprom.clus.betw, method = "adjusted.rand")
compare(N2prom.clus.betw, Wprom.clus.betw, method = "adjusted.rand")
compare(N3prom.clus.betw, Wprom.clus.betw, method = "adjusted.rand")



