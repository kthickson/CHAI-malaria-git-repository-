library(lubridate)
library(ggplot2)

setwd("C:/Users/nr1e14/Google Drive/swaziland")
swazidat=read.csv("swazi.csv",stringsAsFactors=F)
swazidat$newdate = as.Date(swazidat$FirstEntered.1,format = "%d/%m/%Y")
swazidat$newmonth = as.factor(month(swazidat$newdate))


swazidat$newmonth2 = relevel(swazidat$newmonth,ref="6")


summary(glm(data=swazidat,Malaria_0_1 ~ newmonth2))
# Significantly more malaria during Jan, Feb, May, compared to June

swazidat$TravelOS2 = as.numeric(swazidat$TravelOS == "Yes") # shortcut to turn travelos into a binary variable, this will be 1 if true, 0 if false
summary(glm(data=swazidat, TravelOS2 ~ newmonth2,family="binomial"))
#Significantly more travel in Jan and Feb, significantly less in Sept, Oct, Dec

ggplot() + geom_histogram(data=swazidat,aes(x=newmonth,fill=TravelOS),stat="count",bins=12)+
  scale_fill_brewer(palette="Set1",name="Travel outside Swaziland")+
  ylab("Number")+
  xlab("Month")+
  theme_classic() +
  theme(legend.position = "bottom") 

ggplot() + geom_histogram(data=swazidat,aes(x=newmonth,fill=as.factor(Malaria_0_1)),stat="count",bins=12)+
  scale_fill_brewer(palette="Set1",name="Malaria",breaks=c(0,1),labels=c("Negative","Positive"))+
  ylab("Number")+
  xlab("Month")+
  theme_classic() +
  theme(legend.position = "bottom") 


#####xx length of swazi.csv dataset is 11500;length of data.csv dataset is 11627. What is the difference between them? What has been cut in swazi.csv?