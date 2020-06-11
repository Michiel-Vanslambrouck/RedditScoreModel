library(readr)
library(gam)
library(tidyverse)
library(ggplot2)
redd <- read_csv("C:/Users/mcmic/AppData/Roaming/.minecraft/2B2T_folder/reddit/redditout.csv")

summary(redd) #last 1000 posts are from 18/07 to 14/08
redd$time=as.numeric(redd$time)

plot(redd[,c(2,5,8)])
cor.test(redd$score,redd$comms_num) #score and comments are obviously highly correlated

plot(redd$score~redd$created) #seems constant over this last month
plot(redd$score~redd$time) #seems constant throughout the day

#getting more data out of the posts
summary(redd)
head(redd)
dim(redd)

reddd = redd %>% 
  mutate(lentitle = str_length(title))%>% 
  mutate(lenbody = str_length(body))%>% 
  mutate(link = (strtrim(url,15)!="https://www.red"))%>%
  mutate(time = time/10000)
reddd$lenbody[is.na(reddd$lenbody)]=0
summary(reddd)

#linear (GLM) model
model1=glm(score~created + lentitle + lenbody + link + time, data=reddd)
summary(model1) #containing a link is highly significant
par(mfrow=c(1,2))
plot(model1)

#plotting score-time
par(mfrow=c(1,1))
ggplot(reddd,aes(time,log(score+1))) + geom_point() + geom_smooth(method = lm) #shit plot

#nonlinear (GAM) model with smoothing spline
gam1 = gam(score~created + lentitle + lenbody + link + s(time,5), data=reddd) 
#smoothing spline for 24h-time, degrees of freedom = 5
par(mfrow=c(2,3))
plot(gam1,se=TRUE,col="purple",xlab="UTC time",ylab="smoothing spline coef",main="effect of upload time on post score")
#non linear term seems defendable for time
summary(gam1)
#gam1$coefficients #should not be interpreted 




