##패키지 설치
setwd("D:/")
install.packages("scatterplot3d")
install.packages("rgl")
install.packages("DMwR")
install.packages("dummies")
##데이터 불러오기
titanic=read.csv("titanic3.csv")
library(DMwR)
library(dummies)
titanic=centralImputation(titanic)
embarked.dum=dummy(titanic$embarked)
titanic=cbind(titanic,embarked.dum)
sex.dum=dummy(titanic$sex)
titanic=cbind(titanic,sex.dum)
n=nrow(titanic)
titanic1=titanic[,-c(2:3,7,9:13)]
##standardization
titanic1=as.data.frame(scale(titanic1))
attach(titanic1)
##계층군집(3개)
h.clust=hclust(dist(titanic1),method="complete")
plot(h.clust)
rect.hclust(h.clust,3)
h.clust2=hclust(dist(titanic1[1:50,]),method="complete")
plot(h.clust2)
h.cut=cutree(h.clust,k=3)
h.cut
h.cut2=cutree(h.clust,h=7)
h.cut2
h.seg1=titanic1[h.cut==1,]
h.seg2=titanic1[h.cut==2,]
h.seg3=titanic1[h.cut==3,]
pie(table(h.cut),main="number of observations in segment")
h.mean=rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean))
rownames(h.mean)=c(1,2,3)
h.mean
dist(h.mean,method="euclidean",diag=TRUE)
##군집 4개일때
h.cut=cutree(h.clust,k=4)
h.cut
h.seg1=titanic1[h.cut==1,]
h.seg2=titanic1[h.cut==2,]
h.seg3=titanic1[h.cut==3,]
h.seg4=titanic1[h.cut==4,]
pie(table(h.cut),main="number of observations in segment")
h.mean=rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean),apply(h.seg3,2,mean),apply(h.seg4,2,mean))
rownames(h.mean)=c(1,2,3,4)
h.mean
dist(h.mean,method="euclidean",diag=TRUE)
##군집 2개일때
h.cut=cutree(h.clust,k=2)
h.cut
h.seg1=titanic1[h.cut==1,]
h.seg2=titanic1[h.cut==2,]
pie(table(h.cut),main="number of observations in segment")
h.mean=rbind(apply(h.seg1,2,mean),apply(h.seg2,2,mean))
rownames(h.mean)=c(1,2)
h.mean
dist(h.mean,method="euclidean",diag=TRUE)
###k-means 군집
k.clust = kmeans(titanic1[,1:10], centers=3, nstart=20)
k.clust$tot.withinss
k.clust2 = kmeans(titanic1[,1:10], centers=3)
k.clust2$tot.withinss
## Showing the results
pie(k.clust$size, main="number of observations in segment")
k.clust$centers
dist(k.clust$centers, method = "euclidean", diag = TRUE)
##plots
plot3d(titanic1$pclass,titanic1$age,titanic1$fare,col=k.clust$cluster)
scatterplot3d(pclass,age,fare,color=k.clust$cluster)
seg1=titanic1[k.clust$cluster==1,]
seg2=titanic1[k.clust$cluster==2,]
seg3=titanic1[k.clust$cluster==3,]
par(mfrow=c(3,3))
boxplot(seg1[,1],seg2[,1],seg3[,1],ylab=names(seg1)[1],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,2],seg2[,2],seg3[,2],ylab=names(seg1)[2],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,3],seg2[,3],seg3[,3],ylab=names(seg1)[3],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,4],seg2[,4],seg3[,4],ylab=names(seg1)[4],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,5],seg2[,5],seg3[,5],ylab=names(seg1)[5],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,6],seg2[,6],seg3[,6],ylab=names(seg1)[6],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,7],seg2[,7],seg3[,7],ylab=names(seg1)[7],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,8],seg2[,8],seg3[,8],ylab=names(seg1)[8],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,9],seg2[,9],seg3[,9],ylab=names(seg1)[9],xlab="segment",col="blue",names=c(1,2,3))
boxplot(seg1[,10],seg2[,10],seg3[,10],ylab=names(seg1)[10],xlab="segment",col="blue",names=c(1,2,3))
library(scatterplot3d)
####3d 플롯
par(mfrow=c(1,1))
scatterplot3d(titanic1$age,titanic1$sexmale,titanic1$fare,color=k.clust$cluster)
library(rgl)
plot3d(titanic1$age,titanic1$sexmale,titanic1$fare,col=k.clust$cluster)
scatterplot3d(titanic1$parch,titanic1$sexfemale,titanic1$age,color=k.clust$cluster)
plot3d(titanic1$parch,titanic1$sexfemale,titanic1$age,col=k.clust$cluster)
scatterplot3d(titanic1$fare,titanic1$pclass,titanic1$embarkedC,color=k.clust$cluster)
plot3d(titanic1$fare,titanic1$pclass,titanic1$embarkedC,col=k.clust$cluster)

