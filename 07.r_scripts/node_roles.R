####################### node_roles.r ######################
# calculo de roles de nodos de grafos
# parametros fijos
# zc <- 1
# pc <- 0.05

zc <- 1
pc <- 0.05


include.packages <- function (pkg) {
  repos <- "http://cran.r-project.org" 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    print("Installing required packages, please wait...")
    install.packages(new.pkg,repos = repos, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

include.packages("brainGraph")
include.packages("dplyr")
include.packages("igraph")
include.packages("readr")

calcular_roles <- function (g, memb) {
  roles<-data.frame(node=V(g)$name,z=within_module_deg_z_score(g,memb),
                    p=part_coeff(g,memb))
  rownames(roles)<-NULL
  roles <- roles %>% mutate(rol=ifelse(p>pc & z>zc,"Global Hubs",
                                       ifelse(p<pc & z>zc, "Provincial Hubs",
                                              ifelse(p<pc & z<zc, "Provincial Nodes",
                                                     "Connector Nodes"))))
  return(roles)
}


calcular_membresia <- function(M, n, method ="Louvain") {
  
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
  
  if(method == "Optimal"){
    netM.cl.eb <- cluster_louvain(netM)
  }
  
  return(list(grph=netM,membr=netM.cl.eb$membership))
  
}


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


data_reader <- function(path,names,pattern) {
  
  sujetos<-NULL
  i<-1
  files<-list.files(path = path, full.names=T, pattern=pattern)
  
  for (f in files) {
    m1<-read.csv(f,header = F)
    sujetos[i]<-list(as.matrix(m1))
    i<-i+1
  }
  
  promedio <- Reduce('+', sujetos)/length(sujetos)
  colnames(promedio) <- names
  rownames(promedio) <- names
  
  return(promedio)
}
