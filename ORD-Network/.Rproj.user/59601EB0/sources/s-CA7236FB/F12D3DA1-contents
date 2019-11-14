### 
## ORD - PROPOSED REORGANIZATION NETWORK
## This R Script has been created to investigate the network
## Structure of the proposed ORD reorganization
### 
## N.L. Pollesch - pollesch.nathan@epa.gov - ORD/NHEERL/MED

#### Packages and such ####
library(igraph)
library(stringr)
library(readxl)
library(ggraph)

source('ORDNet_functions.R')

#### Create Network ####
## Load Reorg Edge List Workbook
ORD_REORG <- read_excel("data/ORD-REORG.xlsx") #NLP Created this file by hand from https://intranet.ord.epa.gov/about-ord/proposed-reorganization 

## Extract Edge List Object
ORDEL<-as.data.frame(ORD_REORG[,1:2])

## Create igraph object
ORD<-graph_from_edgelist(as.matrix(ORDEL))

## View in igraph
set.seed(1)
par(bg="Black")
plot(ORD)


## Create short names
V(ORD)$longName<-V(ORD)$name
for(i in 1:length(V(ORD))){
  V(ORD)$shortName[i]<-strsplit(V(ORD)$name,split="-")[[i]][V(ORD)$CDB[i]]
}

## Assign Center/Division/Branch status
## 1=Center 2=Div 3=Branch ##NOTE THIS IS BASICALLY 'DEPTH' IN TREE
V(ORD)$CDB<-str_count(names(V(ORD)),"-")+1 

## Find All Branches
V(ORD)[which(V(ORD)$CDB==2)]
## Find Divisions with most branches and sort
rev(sort(degree(ORD,V(ORD)[which(V(ORD)$CDB==2)],mode="out")))

### Calculate centers with most branches
##
brByCen<-list()
noBrByCen<-c()
for(i in 2:length(names(V(ORD)[which(V(ORD)$CDB==1)]))){
  brByCen[[i-1]]<-grep(names(V(ORD)[which(V(ORD)$CDB==1)])[i],names(degree(ORD,V(ORD)[which(V(ORD)$CDB==2)],mode="out")))}
names(brByCen)<-names(V(ORD)[which(V(ORD)$CDB==1)])[2:8]

for(i in 1:length(divByCen)){
  noBrByCen[i]<-sum(degree(ORD,V(ORD)[which(V(ORD)$CDB==2)],mode="out")[brByCen[[i]]])  
}
names(noBrByCen)<-names(brByCen)

### Calculate divisions with the most branchs
##
brByDiv<-list()
noBrByDiv<-c()
for(i in 1:length(names(V(ORD)[which(V(ORD)$CDB==2)]))){
  brByDiv[[i]]<-grep(names(V(ORD)[which(V(ORD)$CDB==2)])[i],names(degree(ORD,V(ORD)[which(V(ORD)$CDB==2)],mode="out")))}
names(brByDiv)<-names(V(ORD)[which(V(ORD)$CDB==2)])

for(i in 1:length(brByDiv)){
  noBrByDiv[i]<-sum(degree(ORD,V(ORD)[which(V(ORD)$CDB==2)],mode="out")[brByDiv[[i]]])  
}
names(noBrByDiv)<-names(brByDiv)

noBrByCen
noBrByDiv

View(distances(ORD))


## Assign colors by C/D/B
CDBcols<-c("seagreen","springgreen","turquoise")
V(ORD)$color<-CDBcols[V(ORD)$CDB]

## Assign size by C/D/B
V(ORD)$size<-(9/V(ORD)$CDB)

## Change Edge Arrow size
E(ORD)$arrow.size<-0
E(ORD)$color<-"gray"

#### Visualize ####
plot(ORD,vertex.label=V(ORD)$shortName,vertex.shape="none",
     edge.curved=.2,  vertex.label.cex=.8,
     vertex.label.color=c("SeaGreen","SpringGreen","Steelblue")[V(ORD)$CDB])

plot(ORD,vertex.label=V(ORD)$shortName,vertex.shape="none",
     edge.curved=.2,  vertex.label.cex=.8, layout=layout_as_tree,
     vertex.label.color=c("SeaGreen","SpringGreen","Steelblue")[V(ORD)$CDB])

par(bg="white")
layoutnew=layout.reingold.tilford(ORD,circular=T)
plot(ORD,vertex.label=V(ORD)$shortName,vertex.shape="none",vertex.size=12,
     edge.curved=0,layout=layoutnew, arrow.size<-0,vertex.label.cex=1/V(ORD)$CDB*.8,
     vertex.label.color=c("SeaGreen","DarkGreen","Steelblue")[V(ORD)$CDB],vertex.color=c("SeaGreen","DarkGreen","Steelblue")[V(ORD)$CDB])

par(bg="white")
layoutnew=layout.reingold.tilford(ORD,circular=T)
plot(ORD,layout=layoutnew, arrow.size<-0,vertex.label.cex=1/V(ORD)$CDB*.8,
     #label.dist=1,
     vertex.label="",
     vertex.label.color=c("SeaGreen","DarkGreen","Steelblue")[V(ORD)$CDB],vertex.color=c("SeaGreen","DarkGreen","Steelblue")[V(ORD)$CDB])



#### VISUALIZATIONS USING GGRAPH ####

ggraph(ORD, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal() + 
  geom_node_point(aes(filter = leaf)) + 
  coord_fixed() + theme_void() + geom_node_point(aes(label = class))


ggraph(ORD, 'circlepack', weight = 'size') + 
  geom_node_circle(aes(fill = depth), size = 0.25, n = 50) + 
  coord_fixed()+ theme_void() 
#+ geom_node_text( aes(label=shortName, filter=leaf, fill=depth, size=size)) 

l <- ggraph(ORD, layout = 'partition', circular = TRUE)

l + geom_node_arc_bar(aes(fill = depth)) + 
  coord_fixed()

# This one takes a bit
l + geom_edge_diagonal(aes(width = ..index.., alpha = ..index..), lineend = 'round') + 
  scale_edge_width(range = c(0.2, 1.5)) + 
  geom_node_point(aes(colour = depth)) + 
  coord_fixed()+ theme_void()

# Circular Layout with Labels
myleaves=which(degree(ORD,mode="out")==0)
nleaves=length(myleaves)
V(ORD)$id[ myleaves ] = seq(1:nleaves)
V(ORD)$angle= 90 - 360 * V(ORD)$id / nleaves
# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
V(ORD)$hjust<-ifelse( V(ORD)$angle < -90, 1, 0)
# flip angle BY to make them readable
V(ORD)$angle<-ifelse(V(ORD)$angle < -90, V(ORD)$angle+180, V(ORD)$angle)
# angles for all non-leaves
V(ORD)$angle[which(is.na(V(ORD)$angle))]<-0

ggraph(ORD, layout = 'dendrogram') + 
  geom_edge_arc(color="gray") +
  geom_node_point(aes(colour=color, shape="circle", size=1/CDB))+
  #geom_node_text(aes( x = x*1.15, y=y*1.15,label=shortName, filter=leaf, angle = angle)) + theme_void() +
  #geom_node_text(aes(label=shortName, filter=!leaf, angle = 0), nudge_y=.1) + 
  theme_void() +
  expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3)) + 
  theme(
    legend.position="none",
    plot.margin=unit(c(0,0,0,0),"cm"))
  


layoutauto<-layout.auto(ORD)


#### DISTANCES ####
summary(distances(ORD))
sum(distances(ORD))
mean(distances(ORD))
table(V(ORD)$CDB)
# Mean Distance of IO to network
mean(distances(ORD)[1,])

# Mean Distance of Branch Chiefs to Network
sum(distances(ORD, v=V(ORD)[which(V(ORD)$CDB==3)],to=V(ORD)))/(length(V(ORD)[which(V(ORD)$CDB==3)])*126)
#### GROUPING EXERCISES ####

## Number of connections by number of groups
group.edges.count.plot(126, pch=18,type="b", xlab="Number of groups",ylab="New connections created")
text(x=70,y=(group.edges.total(group.size(126,quiet=T))+180),labels="Group Size Algorithm: 660 connections created",col="steelblue")


#### ~ Randomly group ORD ####

ORDR<-create.groups(ORD,"random",all=F)

for(i in 1:length(ORDR)){
  plot(ORDR[[i]],vertex.label="",layout=layoutauto)}
table(V(ORDR[[length(ORDR)]])$grp)
table(V(ORDR[[length(ORDR)]])$color)

#### ~ Greedy grouping of ORD ####
ORDG<-create.groups(ORD,"greedy",all=F)

for(i in 1:length(ORDG)){
  plot(ORDG[[i]],vertex.label="",layout=layoutauto)}
table(V(ORDG[[length(ORDG)]])$grp)
table(V(ORDG[[length(ORDG)]])$color)

edge_attr_names(ORDwhite)

ORDwhite<-ORD
V(ORDwhite)$color<-"gray"
E(ORDwhite)$width<-.5
ORDGAll<-create.groups(ORDwhite,"greedy",all=T,ECurve=T)
par(bg="black")
layoutsg<-layout
for(i in 1:length(ORDGAll)){
  plot(ORDGAll[[i]],vertex.frame.color=V(ORDGAll[[i]])$color,vertex.label="",layout=treelay)}
table(V(ORDGAll[[length(ORDGAll)]])$grp)
table(V(ORDGAll[[length(ORDGAll)]])$color)

treelay<-layout_as_tree(ORDwhite)
layoutrt<-layout.reingold.tilford(ORDwhite)
### Animate ###
library(purrr)
library(magick)
library(dplyr)
for(i in 1:length(ORDGAll)){
  par(bg="black")
  png(paste("images/networks/net_",str_pad(i, width=3, side="left", pad="0"),".png",sep=""))
  plot(ORDGAll[[i]],edge.width=1,vertex.frame.color=V(ORDGAll[[i]])$color,vertex.label="",layout=layoutrt,margin=c(.25,0,0,0))
  dev.off()
  }

list.files(path=paste(getwd(),"/images/networks",sep=""),pattern="*.png",full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=5) %>%
  image_write("images/nets_g_tree_morph.gif",quality=100)




#### ~ Shy grouping of ORD ####
#### TO DO - GIVE BACON CHOICE OPTION ####
ORDS<-create.groups(ORD,"shy",all=F)


for(i in 1:length(ORDS)){
  plot(ORDS[[i]],vertex.label="",layout=layoutauto)}
table(V(ORDS[[length(ORDS)]])$grp)
table(V(ORDS[[length(ORDS)]])$color)


ORDSAll<-create.groups(ORDwhite,"shy",all=T,ECurve=T)



### Animate ###
library(purrr)
library(magick)
library(dplyr)
for(i in 1:length(ORDGAll)){
  png(paste("images/networks/net_",str_pad(i, width=3, side="left", pad="0"),".png",sep=""))
  par(bg="black")
  plot(ORDSAll[[i]],vertex.label="",layout=treelay)
  dev.off()
}
list.files(path=paste(getwd(),"/images/networks",sep=""),pattern="*.png",full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=4) %>%
  image_write("images/nets_s_tree.gif")


#### Compare Distances ####

mdR<-c()
mdG<-c()
mdS<-c()
for(i in 1:length(ORDR)){
   mdR[i]<-mean_distance(ORDR[[i]],dir=F)
     mdG[i]<-mean_distance(ORDG[[i]],dir=F)
     mdS[i]<-mean_distance(ORDS[[i]],dir=F)
}
plot(mdR,type="l",col="red")
lines(mdG,col="blue")
lines(mdS,col="green")


#### Look for trends in Approaches ####

## Create lists of outputs
mdRs<-list()
mdGs<-list()
mdSs<-list()
## 100 realizations for each grouping algorithm ##
for(i in 1:100){
  ORDR<-create.groups(ORD,"random",all=F)
  ORDG<-create.groups(ORD,"greedy",all=F)
  ORDS<-create.groups(ORD,"shy",all=F)

## Distance Calculations
  mdR<-c()
  mdG<-c()
  mdS<-c()
  for(j in 1:length(ORDR)){
    mdR[j]<-mean_distance(ORDR[[j]],dir=F)
    mdG[j]<-mean_distance(ORDG[[j]],dir=F)
    mdS[j]<-mean_distance(ORDS[[j]],dir=F)
  }
  mdRs[[i]]<-mdR
  mdGs[[i]]<-mdG
  mdSs[[i]]<-mdS
}

### Working with data to plot more from results ###


RR.df <- data.frame(matrix(unlist(mdRs), nrow=100, byrow=T),stringsAsFactors=FALSE)
RG.df <- data.frame(matrix(unlist(mdGs), nrow=100, byrow=T),stringsAsFactors=FALSE)
RS.df <- data.frame(matrix(unlist(mdSs), nrow=100, byrow=T),stringsAsFactors=FALSE)
names(RR.df)<-(0:11)
names(RS.df)<-(0:11)
names(RG.df)<-(0:11)

boxplot(RR.df,ylim=c(2.5,5),col="tomato")
par(new=T)
boxplot(RG.df,ylim=c(2.5,5),col="seagreen1")
par(new=T)
boxplot(RS.df,ylim=c(2.5,5),col="gray")


plot(mdRs[[1]],type="l",col="tomato", main="Grouping Results by Number of Groups and Algorithm",xlab="Number of Groups",ylab="Mean Network Distance")
for(i in 1:100){
  lines(mdRs[[i]],col="tomato")
  lines(mdGs[[i]],col="seagreen1")
  lines(mdSs[[i]],col="gray")
}
for(i in 1:12){
points(i,y=mean(RR.df[,i]),bg="tomato",col="Black",pch=21)
points(i,y=mean(RG.df[,i]),bg="seagreen1",col="Black",pch=21)
points(i,y=mean(RS.df[,i]),bg="gray",col="Black",pch=21)}
legend("topright",inset=0.02, legend=c("Random Grouping", '"Gregarious" Grouping', '"Shy" Grouping'),
       col=c("tomato", "SeaGreen1","gray"),lwd=1)
legend("bottomleft",inset=0.05, legend=c("Random Grouping - mean", '"Gregarious" Grouping - mean', '"Shy" Grouping - mean'),
      col="black", pt.bg=c("tomato", "SeaGreen1","gray"),pch=c(21))

## Density Plots

plot(density(RR.df[,12]),type="h",main="Mean Distance by Grouping Algorithm",xlab="Mean Network Distance After Grouping Exercise",ylab="Density",col="tomato",ylim=c(0,27),xlim=c(min(c(mdFR,mdFG,mdFS)),max(c(mdFR,mdFG,mdFS))))
lines(density(RG.df[,12]),col="seagreen1",type="h")
lines(density(RS.df[,12]),col="gray",type="h")
legend("topright",inset=0.02, legend=c("Random Grouping", '"Gregarious" Grouping', '"Shy" Grouping'),
       fill=c("tomato", "SeaGreen1","gray"))


#### EXTRAS ####

## Cluster by short random walks
wc<-cluster_walktrap(ORD)
members<-membership(wc)
ORD_wc<-igraph_to_networkD3(ORD,group=members)
forceNetwork(Links=ORD_wc$links,Nodes=ORD_wc$nodes,NodeID='name',Group='group')


#### Make trees ####
d=depth=2
w=width=2
plot(make_tree(sum(w^(0:d)),w))


#### ANIMATE THE NETWORK ####
####~~~ Animation of AOPwiki Growth ####
library(magick)
library(dplyr)
library(purrr)
##Formatting outputs
library(stringr) #supplies a function to add leading zeros to strings (used in the plotting commands)
library(latex2exp) #Allows for TeX commands in plots 
##Library for 3d plotting
library(plot3D)

##Constructive plot of AOPwiki network 
#define layout for network
set.seed(1)
fulllo<-layout.fruchterman.reingold(AOPg)
## Loop for creating successive plots
for(i in 1:153){
  V(isgs[[153]])$color<-NA
  V(isgs[[153]])[!is.na(match(V(isgs[[153]])$KE_EID,V(isgs[[i]])$KE_EID))]$color<-'blue'
  E(isgs[[153]])$color<-NA
  E(isgs[[153]])[match(as_ids(E(isgs[[i]])),as_ids(E(isgs[[153]])))]$color<-'white'
  png(paste("images/networks/net_",str_pad(i, width=3, side="left", pad="0"),".png",sep=""),wid=800,hei=800)
  par(col.main="white",bg="black")
  plot(isgs[[153]],margin=c(-.05,-.05,-.05,-.05),layout=fulllo,vertex.label=NA, vertex.frame.color=NA, edge.arrow.size=.5,vertex.size=2,main="AOPwiki Network Evolution")
  text(0,y=-1.1,paste("-- Network Summary -- \n Key Events:",KEno[i]," Key Event Relationships:",KERno[i]," Linear AOPs:",AOPno[i]),font=2,col="white")
  dev.off()
}


list.files(path=paste(getwd(),"/images/networks",sep=""),pattern="*.png",full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=4,loop=1) %>%
  image_write("images/net_evolution.gif")

## Plot of all AOPwiki sub-networks, does not maintain layout...previous plots do maintain layout
for(i in 1:153){
  png(paste("images/networks/net_",str_pad(i, width=3, side="left", pad="0"),".png",sep=""))
  aplot(isgs[[i+1]],vsize=2)
  dev.off()
}
list.files(path=paste(getwd(),"/images/networks",sep=""),pattern="*.png",full.names=T) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=4) %>%
  image_write("images/nets.gif")



