library(readr)
library(gam)
library(tidyverse)
library(ggplot2)
library(plotrix)
require(lubridate)
library(rpart)
library(MASS)
library(cluster)

# reddit API only allows to scrape 1000 posts at a time, I pulled the last 1000 posts multiple times through the months
redd3 <- read_csv("C:/Users/mcmic/Documents/Files (C)/9) AMSA/reddit/redditout3.csv")
redd4 <- read_csv("C:/Users/mcmic/Documents/Files (C)/9) AMSA/reddit/redditout4.csv")
redd5 <- read_csv("C:/Users/mcmic/Documents/Files (C)/9) AMSA/reddit/redditout5.csv")
redd6 <- read_csv("C:/Users/mcmic/Documents/Files (C)/9) AMSA/reddit/redditout6.csv")
redd=rbind(redd3,redd4,redd5,redd6) # combining everything to one dataset
redd = redd[!duplicated(redd$id),] # duplicate (overlap) filter makes hits go down from 3977 to 3170
summary(redd) # data between 2019-07-17 and 2019-12-31 
redd$time=as.numeric(redd$time) # UTC time
plot(redd$score~redd$created) # one month is missing but data remains consistent
plot(redd$score~redd$time) # seems relatively constant throughout the day

#getting more interesting variables out of the raw posts
head(redd)
clockS = function(t){hour(t)+minute(t)/60+second(t)/3600}
plot(clockS(redd$timestamp))
reddd = redd %>% 
  mutate(lentitle = str_length(title))%>% # the length of a post title may contribute to score
  mutate(capsratio = str_count(title,"[A-Z]")/str_count(title,"[^ ]"))%>% # would excessive caps use improve?
  mutate(lenbody = str_length(body))%>% # maybe longer text stories get more upvotes
  mutate(link = (strtrim(url,15)!="https://www.red"))%>% # external link to image/video present?
  mutate(time = clockS(timestamp)) # a proper float number for UTC time of day so we can work numerically
reddd$lenbody[is.na(reddd$lenbody)]=0
summary(reddd)
#win.graph()
pairs(reddd[c(2,5,10,11,12,13,14)],main='pairwise scatterplots of numeric / binary variables',cex.lab=2)
cor.test(redd$score,redd$comms_num) # score and comments are obviously highly correlated, both are output variables

#PCA on all numerical
x.mat = reddd[c(2,5,10,11,12,13)] # all relevant numerical vars
x.pca = princomp(x.mat, cor=TRUE) # variable scales are wildly different, correlation is prefered over covar matrix
x.pca$sdev^2   # eigenvalues do not show quick descent, dropping dimensions will come at a cost
x.pca$loadings # PC loadings, the most important PC is score and number of comments increasing together 
screeplot(x.pca,main='screeplot for PCA components')
x.pca$scores
summary(x.pca) # 94% of the variation can still be explained if we drop the last PC, poor dimension reduction here

#maximum likelihood FA on all numerical
x.fmle <- factanal(x.mat,factors=3,method="mle")
x.fmle
x.fmle <- factanal(x.mat,factors=3,method="mle",rotation="varimax")
x.fmle
scores=factanal(x.mat,factors=3,method="mle",rotation="varimax",scores = "regression")$scores

#Biplot
subdat.matrix<-as.matrix(x.mat)
attributes(subdat.matrix)
vnames<-dimnames(subdat.matrix)[[2]]
dimnames(subdat.matrix)[[1]]<-c(1:nrow(subdat.matrix))
PCA.biplot<- function(x) {
  xm<-apply(x,2,mean)
  y<-sweep(x,2,xm)
  ss<-(t(y)%*%y)
  s<-ss/(nrow(x)-1)
  d<-(diag(ss))^(-1/2)
  e<-diag(d,nrow=ncol(x),ncol=ncol(x))
  z<-y%*%e
  r<-t(z)%*%z
  q<-svd(z)
  gfd<-((q$d[1])+(q$d[2]))/sum(q$d)
  gfz<-(((q$d[1])^2)+((q$d[2])^2))/sum((q$d)^2)
  gfr<-(((q$d[1])^4)+((q$d[2])^4))/sum((q$d)^4)
  l<-diag(q$d,nrow=ncol(x),ncol=ncol(x))
  R.B<-q$u        #scores matrix
  C.B<-q$v%*%l    #loadings
  scalefactor<-5  #stretch scores by a scale factor
  R.B<-q$u *scalefactor
  par(mar=c(4,4,4,4),pty='s',oma=c(5,0,0,0),font=2)
  plot(R.B[ ,1],R.B[ ,2],axes=F,xlim=c(-1,1),ylim=c(-1,1),xlab=' ',ylab=' ',pch='.',cex=.8)
  mtext('First component',side=1,line=3,cex=.8)
  mtext('Second component',side=2,line=3,cex=.8)
  axis(1,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  axis(2,at=c(-1,-.8,-.6,-.4,-.2,0,.2,.4,.6,.8,1),cex=.8)
  box( )
  points(C.B[,1],C.B[,2],pch=".")
  text(C.B[,1]-.05,C.B[,2]+.05,as.character(dimnames(x)[[2]]),cex=0.8)
  for (i in seq(1,nrow(C.B),by=1)){arrows(0,0,C.B[i,1],C.B[i,2])}
  draw.circle(0,0,1,border='black')
  mtext('PCA Biplot',side=1,outer=T,cex=1,line=3)
  results<-list('correlation matrix'=r,'column effects'=C.B,'row effects'=R.B)
  cat('The goodness of fit for the correlation matrix is',gfr,'for the centered, standardized design matrix',gfz,'and for the Mahalanobis distances is',gfd,' ') 
  results
}
PCA.biplot(subdat.matrix)

#decision tree
red.tree <- rpart(score ~ time+lentitle+lenbody+link+capsratio, data=reddd)
summary(red.tree)
red.tree
win.graph()
plot(red.tree,uniform=F)         
text(red.tree,splits=T,all=T) 
title("unpruned decision tree")
printcp(red.tree)
plotcp(red.tree) #tree size 4 does not offer much additional value compared to size 2
#plotting cost complexity in reation to number of splits
plot(red.tree$cptable[,2],red.tree$cptable[,1],xlab='Number of splits',ylab='Cost complexity parameter,cp')
#pruning trees at specific cost complexity cp
pruned.tree<-prune(red.tree,cp=0.02) 
summary(pruned.tree)
pruned.tree 
plot(pruned.tree)
text(pruned.tree)
title("pruned tree at cp=0.02")

#clustering
red.clus <- hclust(dist(x.mat), method="average")  # AVARAGE LINK
# plclust plots the dendrogram for the cluster object.
plclust(red.clus,xlab="Reddit Data",ylab="Average Link Distance", sub="")
# cutree finds the cluster groupings for a cluster object for a 
# specifed (k) number of clusters.
red.gp <- cutree(red.clus,k=7)
# You might want to see how many observations in each cluster.
table(red.gp)
plot(reddd$score,reddd$lenbody, col = red.gp, cex=1.5,main="Average linkage clusters k=7")
pairs(reddd[c(2,5,10,11,12,13,14)],col = red.gp,main='pairwise scatterplots with clusters',cex.lab=2)
clusplot(x.mat,red.gp,stand=TRUE,labels=0,main="Clusplot average linkage")

#linear (GLM) model
model1=glm(score~lentitle + lenbody + capsratio + link + time, data=reddd)
summary(model1) #containing a link is highly significant
par(mfrow=c(1,2))
plot(model1)
#plotting score-time
par(mfrow=c(1,1))
ggplot(reddd,aes(time,log(score+1))) + geom_point() + geom_smooth(method = lm) #i wish this promised more
#nonlinear (GAM) model with smoothing spline
gam1 = gam(score~lentitle + lenbody + capsratio + link + s(time,2), data=reddd) 
#smoothing spline for 24h-time, degrees of freedom = 2
par(mfrow=c(2,3))
plot(gam1,se=TRUE,col="purple")
#non linear term seems defendable for time
summary(gam1)
#gam1$coefficients #should not be interpreted numerically


