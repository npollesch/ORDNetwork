

#### Get packages and functions
source('ORDNet_functions.R')
#install.packages("vegan")
#install.packages("matrixStats")
library(vegan)
library(readxl)
library(dplyr)
library(matrixStats)


#### Read in data
MGRS<- read_excel("data/MGRS_clean.xlsx")
View(MGRS)

#### Generate Random Groups
nreps<-100001

JsMin<-JsMean<-JsTot<-divsTot<-1:nreps
JsObj<-0
ObjRep<-0
divObj<-0
for(reps in 1:nreps){
groups<-group.size(length(MGRS$Name),quiet=T)
peeps<-ifelse(is.null(dim(groups)),groups[1]*groups[2],(groups[1,1]*groups[1,2]+groups[2,1]*groups[2,2]))
rsamp<-sample(1:peeps)

grpMat1<-matrix(rsamp[1:(groups[1,1]*groups[1,2])],nrow=groups[1,1])
grpMat2<-matrix(rsamp[(groups[1,1]*groups[1,2]+1):peeps],nrow=groups[2,1])

teams1<-matrix(data=NA,nrow=groups[1,1],ncol=groups[1,2])
teams1l<-matrix(data=NA,nrow=groups[1,1],ncol=groups[1,2])
for(i in 1:groups[1,1]){
  teams1[i,]<-MGRS$Name[grpMat1[i,]]
  teams1l[i,]<-MGRS$Lvl[grpMat1[i,]]}
teams2<-matrix(data=NA,nrow=groups[2,1],ncol=groups[2,2])
teams2l<-matrix(data=NA,nrow=groups[2,1],ncol=groups[2,2])
for(i in 1:groups[2,1]){
  teams2[i,]<-MGRS$Name[grpMat2[i,]]
  teams2l[i,]<-MGRS$Lvl[grpMat2[i,]]}

#### Score Random Grouping For Evenness

for(i in 1:4){
teamLvlCts[1:groups[1,1],i]<-rowCounts(teams1l,value=i)
teamLvlCts[(groups[1,1]+1):(groups[1,1]+groups[2,1]),i]<-rowCounts(teams2l,value=i)}

q<-colSums(teamLvlCts)/sum(colSums(teamLvlCts))
div<-c()
for(i in 1:(groups[1,1]+groups[2,1])){
  ifelse(i<groups[1,1]+1,div[i]<-sum((teamLvlCts[i,]/groups[1,2])*log((teamLvlCts[i,]/groups[1,2])/q),na.rm=T),div[i]<-sum((teamLvlCts[i,]/groups[2,2])*log((teamLvlCts[i,]/groups[2,2])/q),na.rm=T))
}
divsTot[reps]<-sum(div)

if(reps==1){divObj<-divsTot[reps]}
if(reps>1){if(divsTot[reps]<divObj){
 divObj<-divsTot[reps]
 ObjRep<-reps
 teams1obj<-teams1
 teams1lobj<-teams1l
 teams2obj<-teams2
 teams2lobj<-teams2l
}}
# 
# 
# H<-diversity(teamLvlCts)
# J<-H/log(dim(teamLvlCts)[2])
# 
# JsMean[reps]<-mean(J,na.rm=T)
# JsMin[reps]<-min(J,na.rm=T)
# JsTot[reps]<-sum(J,na.rm=T)
# if(reps==1){JsObj<-JsTot[reps]}
# if(!is.na(min(J))){
# if(reps>1){if(JsTot[reps]>JsObj){
#  JsObj<-JsTot[reps]
#  ObjRep<-reps
#  teams1obj<-teams1
#  teams1lobj<-teams1l
#  teams2obj<-teams2
#  teams2lobj<-teams2l
# }}}
}

#### View output from random groupings

for(i in 1:4){
  teamLvlCts[1:groups[1,1],i]<-rowCounts(teams1lobj,value=i)
  teamLvlCts[(groups[1,1]+1):(groups[1,1]+groups[2,1]),i]<-rowCounts(teams2lobj,value=i)}

H<-diversity(teamLvlCts)
J<-H/log(4)

H
sum(J)

q<-colSums(teamLvlCts)/sum(colSums(teamLvlCts))
div<-c()
for(i in 1:(groups[1,1]+groups[2,1])){
  ifelse(i<groups[1,1]+1,div[i]<-sum((teamLvlCts[i,]/groups[1,2])*log((teamLvlCts[i,]/groups[1,2])/q),na.rm=T),div[i]<-sum((teamLvlCts[i,]/groups[2,2])*log((teamLvlCts[i,]/groups[2,2])/q),na.rm=T))
}
divsTotMax<-sum(div)
divsTotMax
#### Create Output to Export
par(mfrow=c(1,1))
plot(divsTot,xlab="Realization",ylab="Total KL Divergence")
plot(JsTot,xlab="Realization",ylab="Total Evenness")
abline(h=divsTotMax,col="green")

par(mfrow=c(1,2))
image(teams1lobj,axes=F,main="Teams of 13 by Management Level",col=rainbow(5)[2:5])
mtext(text=c(paste("Team",1:10)), side=1, line=0.3, at=seq(0,1,length.out=10), las=2, cex=0.8)
mtext(text=c(paste("Person",1:13)), side=2, line=0.3, at=seq(0,1,length.out=13), las=1, cex=0.8)

image(teams2lobj,axes=F,main="Teams of 14 by Management Level",col=rainbow(5)[2:5])
mtext(text=c(paste("Team",11:13)), side=1, line=0.3, at=seq(0,1,length.out=3), las=2, cex=0.8)
mtext(text=c(paste("Person",1:14)), side=2, line=0.3, at=seq(0,1,length.out=14), las=1, cex=0.8)

teams1out<-matrix(paste(teams1obj,teams1lobj,sep=" - "),nrow=10,ncol=13)
teams2out<-matrix(paste(teams2obj,teams2lobj,sep=" - "),nrow=3,ncol=14)

write.csv(teams1out,file="teams1KL.csv")
write.csv(teams2out,file="teams2KL.csv")
write.csv(J,file="EvennessOutForKL.csv")
