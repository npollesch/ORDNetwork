library(readxl)
All_Attendees_Clean <- read_excel("All-Attendees-Clean.xlsx")
View(All_Attendees_Clean)
MGRS<-All_Attendees_Clean

groups<-group.size(length(MGRS$Name))
peeps<-ifelse(is.null(dim(groups)),groups[1]*groups[2],(groups[1,1]*groups[1,2]+groups[2,1]*groups[2,2]))
rsamp<-sample(1:peeps)

grpMat1<-matrix(rsamp[1:(groups[1,1]*groups[1,2])],nrow=groups[1,1])
grpMat2<-matrix(rsamp[(groups[1,1]*groups[1,2]+1):peeps],nrow=groups[2,1])

teams1<-matrix(data=NA,nrow=groups[1,1],ncol=groups[1,2])
for(i in 1:groups[1,1]){
  teams1[i,]<-MGRS$Name[grpMat1[i,]]}
View(teams1)
teams2<-matrix(data=NA,nrow=groups[2,1],ncol=groups[2,2])
for(i in 1:groups[2,1]){
  teams2[i,]<-MGRS$Name[grpMat2[i,]]}
View(teams2)

write.csv(teams1,file="teams1.csv")
write.csv(teams2,file="teams2.csv")

# dim(MGRS)
# for(i in 1:dim(MGRS)[1]){
# MGRS$Team[i]<-which(teams2==MGRS$Name[i],arr.ind=T)[1]+11}
# 
# table(MGRS$Team)
# MGRS$Team<-NA
# for(i in 1:dim(teams1)[1]){
#   MGRS$Team[match(MGRS$Name,teams1[i,])]<-i
# }
# 
# dim(teams2)[1]
# for(i in 1:dim(teams2)[1]){
#   MGRS$Team[match(teams2[i,],MGRS$Name)]<-i+(dim(teams1)[1])
# }
# 
# table(MGRS$Team)
# teams1
# View(teams2)


