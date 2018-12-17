jk.modularity <- function(M, n, method) {

  diag(M)<-0
  tmp<-sort(as.vector(M),decreasing = TRUE)
  ro = tmp[n]
  M.b = (M>ro)
  netM <- graph.adjacency(M.b,mode="undirected")
  
  if(method == "Girvan-Newman"){
    netM.cl.eb <- cluster_edge_betweenness(netM, directed = F, merges = T)
  }
  
  if(method == "Louvain"){
    netM.cl.eb <- cluster_louvain(netM)
  }
  
  
  mout = modularity(netM,netM.cl.eb$membership)
  return(mout)	
  
  }
  