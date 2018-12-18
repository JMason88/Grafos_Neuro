rm(list=ls())

source('07.r_scripts/LibraryRequireInstaller.r')
source('07.r_scripts/data_reader.r')
source('jk.modularity.R')

libraryRequireInstall('igraph')
libraryRequireInstall('visNetwork')
libraryRequireInstall('ggplot2')



####################### Data Loading #######################

N1 <- data_reader(x = "N1")
N2 <- data_reader(x = "N2")
N3 <- data_reader(x = "N3")
W <- data_reader(x = "W_")

############################################################


######################## Modularity ########################

N = dim(N1)[1]
Nmaxlinks = N*(N-1)
nlist = seq(100,2000,100)
dlist = array(data=NA, dim=length(nlist))
W.mlist = array(data=NA, dim=length(nlist))
N1.mlist = array(data=NA, dim=length(nlist))
N2.mlist = array(data=NA, dim=length(nlist))
N3.mlist = array(data=NA, dim=length(nlist))
k = 0
for (n in nlist) {
  k = k+1
  dlist[k] = n/Nmaxlinks
  W.mlist[k] = jk.modularity(W,n,"Louvain")
  N1.mlist[k] = jk.modularity(N1,n,"Louvain")
  N2.mlist[k] = jk.modularity(N2,n,"Louvain")
  N3.mlist[k] = jk.modularity(N3,n,"Louvain")
}

plot.communities(jk.modularity(W,n,"Louvain"))

df <- data.frame(dlist,W.mlist,N1.mlist,N2.mlist,N3.mlist)



ggplot(df, aes(dlist)) +                    # basic graphical object
  geom_line(aes(y=W.mlist), colour="black") +  # first layer
  geom_line(aes(y=N1.mlist), colour="green")  + # second layer
  geom_line(aes(y=N2.mlist), colour="blue")  + # second layer
  geom_line(aes(y=N3.mlist), colour="red") +  # second layer
  theme_minimal()
