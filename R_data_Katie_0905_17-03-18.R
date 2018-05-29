#Install packages
#Input data
#Define datasets
#Alter variables and create new variables
#Define additional datasets to be used 
#Explore variables 
#Model 

#install packages#
install.packages("knitr")
install.packages("dplyr")
install.packages("pander")
install.packages("lme4")
install.packages("stargazer")##for summarising models
install.packages("geosphere")##for calculating distance
install.packages("tidyverse")


library(dplyr)
library(knitr)
library(pander)
library(lme4)
library(stargazer)
library(geosphere)
library(tidyverse)

#call data#

datain<-read.csv("/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data.csv", header=T)

##subset the input data ##
###according to DID travel###

data1 <- subset(datain, Travel_in_out ==2) ##travel in
length(data1$ID)
data2 <- subset(data1, uniqueTR_In ==1) ##unique records
length(data2$ID)

data3 <- subset(datain, Travel_in_out ==1) ##travel out
length(data3$ID)
data4 <- subset(data3, uniqueTR_Out ==1) ##unique records
length(data4$ID)


###according to DID NOT travel###

data5 <- subset(datain,Travel_in_out ==0)
length(data5$ID)
data6 <- subset(data5, unique.records ==1)
length(data6$ID)

###all dataset unique records###

data7 <- subset(datain, unique.records ==1)
length(data7$ID)

##create new variables and datasets## 

###travel distance variables###
####Distance variable calculated using Lon_EA_Home and lat_EA_Home, lat_to and lon_to (n.b alternatives include: Lat_from_orig and Lon_from_orig)
lat_home_in<-(subset(data2,is.na(lat_to)==FALSE)$lat_EA_HOME)
length(lat_home_in)
lon_home_in<-(subset(data2,is.na(lat_to)==FALSE)$Lon_EA_HOME)
length(lon_home_in)
lat_travel_in<-subset(data2,is.na(lat_to)==FALSE)$lat_to
length(lat_travel_in)
lon_travel_in<-subset(data2,is.na(lat_to)==FALSE)$lon_to
length(lon_travel_in)
coordinate_table_in<- data.frame(lon_home_in,lat_home_in,lon_travel_in,lat_travel_in)
home_coordinate_data_in<-data.frame(lon_home_in,lat_home_in)
travel_coordinate_data_in<-data.frame(lon_travel_in,lat_travel_in)
distance_in<-geosphere::distHaversine(home_coordinate_data_in,travel_coordinate_data_in)
which(is.na(lat_travel_in)) # should be 0

lat_home_out<-(subset(data4,is.na(lat_to)==FALSE)$lat_EA_HOME)
length(lat_home_out)
lon_home_out<-(subset(data4,is.na(lat_to)==FALSE)$Lon_EA_HOME)
length(lon_home_out)
lat_travel_out<-subset(data4,is.na(lat_to)==FALSE)$lat_to
length(lat_travel_out)
lon_travel_out<-subset(data4,is.na(lat_to)==FALSE)$lon_to
length(lon_travel_out)
coordinate_table_out<- data.frame(lon_home_out,lat_home_out,lon_travel_out,lat_travel_out)
home_coordinate_data_out<-data.frame(lon_home_out,lat_home_out)
travel_coordinate_data_out<-data.frame(lon_travel_out,lat_travel_out)
distance_out<-geosphere::distHaversine(home_coordinate_data_out,travel_coordinate_data_out)
which(is.na(lat_travel_out)) # should be 0


###dataset with distance variable###
data2_noNA_lon<-subset(data2,is.na(lon_to)==FALSE)##remove NAs from lon_to, so distance can be added
length(data2_noNA_lon$ID)
data2_with_distance<- data.frame(data2_noNA_lon, distance_in,log(distance_in))#table to use as data in glm
length(data2_with_distance$ID)
####use which(0 == data2$Malaria_0_1) or which (is.na(data2$age_class) if necessary here

data4_noNA_lon<-subset(data4,is.na(lon_to)==FALSE)##remove NAs from lon_to, so distance can be added 
length(data4_noNA_lon$ID)
data4_with_distance<- data.frame(data4_noNA_lon, distance_out,log(distance_out))#table to use as data in glm
length(data4_with_distance$ID)#shorter because there are 66 NAs for lat_to and lon_to
####use which(0 == data4_noNA_lon_lat$Malaria_0_1) or which (is.na(data4_noNA_lon_lat$age_class) if necessary here

####data with distance variable by occupation code
data4_with_distance_occupation_code1=subset(data4_with_distance,Occupation_code==1)
length(data4_with_distance_occupation_code1$ID)
data4_with_distance_occupation_code2=subset(data4_with_distance,Occupation_code==2)
length(data4_with_distance_occupation_code2$ID)
data4_with_distance_occupation_code3=subset(data4_with_distance,Occupation_code==3)
length(data4_with_distance_occupation_code3$ID)
data4_with_distance_occupation_code4=subset(data4_with_distance,Occupation_code==4)
length(data4_with_distance_occupation_code4$ID)
data4_with_distance_occupation_code5=subset(data4_with_distance,Occupation_code==5)
length(data4_with_distance_occupation_code5$ID)
data4_with_distance_occupation_code6=subset(data4_with_distance,Occupation_code==6)
length(data4_with_distance_occupation_code6$ID)
data4_with_distance_occupation_code7=subset(data4_with_distance,Occupation_code==7)
length(data4_with_distance_occupation_code7$ID)
data4_with_distance_occupation_code8=subset(data4_with_distance,Occupation_code==8)
length(data4_with_distance_occupation_code8$ID)
data4_with_distance_occupation_code9=subset(data4_with_distance,Occupation_code==9)
length(data4_with_distance_occupation_code9$ID)
data4_with_distance_occupation_code10=subset(data4_with_distance,Occupation_code==10)
length(data4_with_distance_occupation_code10$ID)
data4_with_distance_occupation_codeNA=subset(data4_with_distance,is.na(Occupation_code)==TRUE)
length(data4_with_distance_occupation_codeNA$ID)#these should total length(data4_with_distance$ID)

####give occupation code subset datasets unique distance_out names for use in ggplot of distance distributions
colnames(data4_with_distance_occupation_code1)[216]<-"distance_out_occupation1"
colnames(data4_with_distance_occupation_code2)[216]<-"distance_out_occupation2"
colnames(data4_with_distance_occupation_code3)[216]<-"distance_out_occupation3"
colnames(data4_with_distance_occupation_code4)[216]<-"distance_out_occupation4"
colnames(data4_with_distance_occupation_code5)[216]<-"distance_out_occupation5"
colnames(data4_with_distance_occupation_code6)[216]<-"distance_out_occupation6"
colnames(data4_with_distance_occupation_code7)[216]<-"distance_out_occupation7"
colnames(data4_with_distance_occupation_code8)[216]<-"distance_out_occupation8"
colnames(data4_with_distance_occupation_code9)[216]<-"distance_out_occupation9"
colnames(data4_with_distance_occupation_code10)[216]<-"distance_out_occupation10"


###datasets with falciparum_prevalence variable###
####use for all falciparum_prevalence analysis

data4_noNA_falciparum_prevalence<-subset(data4,is.na(falciparum_travel_prevalence)==FALSE)#remove NAs from falciparum prevalence
length(data4_noNA_falciparum_prevalence$ID)#number of falciparum non-NA values in data4
length((subset(data4,is.na(falciparum_travel_prevalence)==TRUE))$ID)#total number of NAs in data4 falciparum travel prevalence
length(subset(data4,fal_12==-9999)$ID)#number of data4 missing falciparum values due to missing MAP falciparum values 
length(subset(data4,is.na(Year.travel))$ID)#number of data4 missing falciparum values due to missing 'Year.travel' in data
###some falcip values missing from data4 due to a)missing year travel value and b)mising malaria atlas project data.


###occupation variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving occupation_code 
data2_selected_occupations<-  factor(data2$Occupation_code, levels=c('1','2','3','4','5','6','8','9'))
data4_selected_occupations<-  factor(data4$Occupation_code, levels=c('1','2','3','4','5','6','8','9')) 
data2_noNA_lon_selected_occupations<-  factor(data2_noNA_lon$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data2_with_distance_selected_occupations<-  factor(data2_with_distance$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data4_noNA_lon_selected_occupations<-  factor(data4_noNA_lon$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data4_with_distance_selected_occupations<-  factor(data4_with_distance$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data4_noNA_falciparum_prevalence_selected_occupations<- factor(data4_noNA_falciparum_prevalence$Occupation_code, levels=c('1','2','3','4','5','6','8')) 

###country variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_selected_countries<-  factor(data4$Country, levels=c("Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Unknown","Zambia","Zimbabwe")) 
data4_noNA_lon_selected_countries<-  factor(data4_noNA_lon$Country, levels=c( "Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Zambia","Zimbabwe")) 
data4_with_distance_selected_countries<-  factor(data4_with_distance$Country, levels=c( "Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Zambia","Zimbabwe")) 
data4_noNA_falciparum_prevalence_selected_countries<- factor(data4_noNA_falciparum_prevalence$Country, levels=c( "Equitorial Guinea","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Zambia","Zimbabwe")) 


###gender variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_selected_genders<-  factor(data4$Gender, levels=c("M","F"))


###manual workers dataset###
data4_manual_workers<- subset(data4,Occupation_code==5)
data4_manual_workers



#Exploring variables#

##distance_in##
hist(log(distance_in),40,freq=FALSE,	main="Histogram of log(distance_in) to show normal distribution",col="lightcoral");

xseq<-seq(min(log(distance_in)),max(log(distance_in)),0.01)
densities<-dnorm(xseq,mean(log(distance_in)),sd(log(distance_in)))
plot(xseq, densities, col="darkgreen",xlab="Sequence in log(distance_in) range", ylab="Density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'Distance' sample size 
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)

####Extension: add a line showing an ideal distribution (e.g. the normal distribution) with mean and standard deviation estimated from the sample
####Extension: add normal distribution curve on top of frequency distribution curve 
####(see https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r) e.g. 
####xseq<-seq(min(distance),max(distance),100)
####curve(dnorm(xseq, mean=mean(distance), sd=sd(distance)), col="darkblue", lwd=2)


##age_out##
age<- table(data4$Malaria_0_1, data4$age_class)#table to view counts in each class
age 

##Distance_out##


hist(log(distance_out),40,freq=FALSE,	main="Histogram of log(distance_out) to show normal distribution",col="lightcoral");

xseq<-seq(min(log(distance_out)),max(log(distance_out)),0.01)
densities<-dnorm(xseq,mean(log(distance_out)),sd(log(distance_out)))
plot(xseq, densities, col="darkgreen",xlab="Sequence in distance range", ylab="Probability density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'Distance_out' sample size
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)

##Falciparum_travel_prevalence##

hist((data4_noNA_falciparum_prevalence$falciparum_travel_prevalence),10,freq=FALSE,	main="Histogram of log(distance_out) to show normal distribution",col="lightcoral");

xseq<-seq(from=min(data4_noNA_falciparum_prevalence$falciparum_travel_prevalence,na.rm=TRUE),to=max(data4_noNA_falciparum_prevalence$falciparum_travel_prevalence,na.rm=TRUE),0.01)
densities<-dnorm(xseq,mean(data4_noNA_falciparum_prevalence$falciparum_travel_prevalence,na.rm=TRUE),sd(data4_noNA_falciparum_prevalence$falciparum_travel_prevalence,na.rm=TRUE))
plot(xseq, densities, col="darkgreen",xlab="Sequence in travel_prevalence range", ylab="Probability density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'travel_prevalence' sample size 
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)

#### xx Extension: add lines showing an ideal distribution (e.g. the normal distribution) with mean and standard deviation estimated from the sample 
#### xx Extension: add normal distribution curve on top of frequency distribution curve 
####(see https://stackoverflow.com/questions/20078107/overlay-normal-curve-to-histogram-in-r) e.g. 
####xseq<-seq(min(distance),max(distance),100)
####curve(dnorm(xseq, mean=mean(distance), sd=sd(distance)), col="darkblue", lwd=2)




#####################
#Descriptive stats#

#####################

##Breakdown of "Other" occupations

library(ggplot2)
library(scales)
data4_occupation_other_all=as.data.frame(table(data4$Occupation.other))
grouped_occupation_other_less_than_Freq_of_1=subset(data4_occupation_other_all,Freq<=1)
data4_occupation_other_selected_values=subset(data4_occupation_other_all,Freq>=2 & Freq<1000)
data4_occupation_other_selected_proportions=as.data.frame(prop.table(data4_occupation_other_selected_values[,2]))
data4_occupation_other_selected_percents=as.data.frame(percent(data4_occupation_other_selected_proportions[,1]))
data4_occupation_other_selected_compiled=data.frame(c(data4_occupation_other_selected_values,data4_occupation_other_selected_proportions,data4_occupation_other_selected_percents))

barchart_other_occupations=ggplot(data=data4_occupation_other_selected_compiled, aes(x=Var1,y=Freq)) + 
  geom_bar(stat="identity")
barchart_other_occupations
barchart_other_occupations_stacked=ggplot(data=data4_occupation_other_selected_compiled, aes(x=factor(1),y=Freq,fill=factor(Var1))) + 
  geom_bar(stat="identity", width=1) +
  labs(title="Breakdown of 'Other' occupations by frequency for \ncases who have travelled outside of Swaziland",caption="Source: xx") +
  theme(text=element_text(size=8, face='plain')) +
  theme(plot.title = element_text(size=rel(1.5), face="bold")) + 
  theme(axis.title = element_text(size=rel(1.2)))
barchart_other_occupations_stacked 
ggsave("barchart_other_occupations_stacked.pdf")
pie_other_occupations<-barchart_other_occupations_stacked+coord_polar(theta = "y" ) + 
  ylab("Frequency of occupation for cases who \ntravelled outside Swaziland") + 
  xlab("") + 
  labs(fill="'Other' occupations") 
pie_other_occupations
ggsave("pie_other_occupations.pdf")

##proportion of manual workers travelling who spent less than 30 days away 
data4_manual_workers_few_days_away<-subset(data4_manual_workers,nightsaway<30)
length(data4_manual_workers_few_days_away$ID)
data4_manual_workers_many_days_away<-subset(data4_manual_workers,nightsaway>=30)
length(data4_manual_workers_many_days_away$ID)
length(data4_manual_workers_few_days_away$ID)/(length(data4_manual_workers_few_days_away$ID)+length(data4_manual_workers_many_days_away$ID))#proportion

##proportion of manual workers travelling who spent less than 30 days away 
data4_manual_workers_Jan_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Jan_travel<-length(data4_manual_workers_Jan_travel$ID)
data4_manual_workers_Feb_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Feb_travel<-length(data4_manual_workers_Feb_travel$ID)
data4_manual_workers_Mar_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Mar_travel<-length(data4_manual_workers_Mar_travel$ID)
data4_manual_workers_Apr_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Apr_travel<-length(data4_manual_workers_Apr_travel$ID)
data4_manual_workers_May_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_May_travel<-length(data4_manual_workers_May_travel$ID)
data4_manual_workers_Jun_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Jun_travel<-length(data4_manual_workers_Jun_travel$ID)
data4_manual_workers_Jul_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Jul_travel<-length(data4_manual_workers_Jul_travel$ID)
data4_manual_workers_Aug_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Aug_travel<-length(data4_manual_workers_Aug_travel$ID)
data4_manual_workers_Sep_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Sep_travel<-length(data4_manual_workers_Sep_travel$ID)
data4_manual_workers_Oct_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Oct_travel<-length(data4_manual_workers_Oct_travel$ID)
data4_manual_workers_Nov_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Nov_travel<-length(data4_manual_workers_Nov_travel$ID)
data4_manual_workers_Dec_travel<-subset(data4_manual_workers,Month.travel==1)
Manual_Dec_travel<-length(data4_manual_workers_Jan_travel$ID)

Manual_Jan_travel+Manual_Feb_travel+Manual_Mar_travel+Manual_Apr_travel+Manual_May_travel+Manual_Jun_travel+Manual_Jul_travel+Manual_Aug_travel+Manual_Sep_travel+Manual_Oct_travel+Manual_Nov_travel+Manual_Dec_travel
Manual_Sep_travel+Manual_Oct_travel+Manual_Nov_travel+Manual_Dec_travel
(Manual_Sep_travel+Manual_Oct_travel+Manual_Nov_travel+Manual_Dec_travel)/(Manual_Jan_travel+Manual_Feb_travel+Manual_Mar_travel+Manual_Apr_travel+Manual_May_travel+Manual_Jun_travel+Manual_Jul_travel+Manual_Aug_travel+Manual_Sep_travel+Manual_Oct_travel+Manual_Nov_travel+Manual_Dec_travel
)#no more than expected manual workers travel during sugar harvesting season sept to dec 


########################

#model TRAVEL IN#
#######################


####here's how to interpret results
#malaria_age.IN<-glm(as.factor(Malaria_0_1) ~ as.factor(age_class), data = data2, family=binomial(link=logit)) # first model, predicting using age class
#malaria_age.IN - this shows coefficients for each value of x 
#summary(malaria_age.IN) - this shows probability that the z value obtained for each coefficient would have occurred if H0 (no relationship) is true
#coef.age.IN<-coef(malaria_age.IN) - just takes coefficients only with no other rubbish!
#exp.age.IN<- exp(coef(malaria_age.IN)) - this shows odds ratio for each value of x compared with the scenario in which that x variable is zero and only the x value used as base value (intercept) is used
#confint(malaria_age.IN, level=0.95) - this shows the coefficient values with 5% and 95% confidence intervals
#conf.age.IN<-exp(confint(malaria_age.IN)) - this shows the odds ratio for the confidence intervals for each variable compared with base variable 


##Modelling malaria##

###null model travel in###
#####this model can be used to compare to other models created
null.model.IN<- glm(Malaria_0_1~1, data = data2,family=binomial(link=logit)) #xx nick - why the ~1?
extractAIC(null.model.IN)

###run glm###
malaria_age.IN<-glm(as.factor(Malaria_0_1) ~ as.factor(age_class), data = data2, family=binomial(link=logit)) # first model, predicting using age class
malaria_age.IN
summary(malaria_age.IN)
coef.age.IN<-coef(malaria_age.IN)
exp.age.IN<- exp(coef(malaria_age.IN))
confint(malaria_age.IN, level=0.95)
conf.age.IN<-exp(confint(malaria_age.IN))

malaria_occupation.IN<-glm(as.factor(Malaria_0_1) ~ data2_selected_occupations, data = data2, family=binomial(link=logit))#xx check warnings here
malaria_occupation.IN
summary(malaria_occupation.IN)
coef.occupation.IN<-coef(malaria_occupation.IN)
exp.occupation.IN<-exp(coef(malaria_occupation.IN))
confint(malaria_occupation.IN, level=0.95)
conf.occupation.IN<-exp(confint(malaria_occupation.IN))
#######check warnings

malaria_distance.IN.null<-glm(as.factor(Malaria_0_1) ~ log(distance_in), data = data2_with_distance, family=binomial(link=logit)) # malaria modelled by distance
malaria_distance.IN.null
summary(malaria_distance.IN.null)
coef.malaria_distance.IN.null<-coef(malaria_distance.IN.null)
coef.malaria_distance.IN.null
exp.malaria_distance.IN.null<- exp(coef(malaria_distance.IN.null))
exp.malaria_distance.IN.null 
confint(malaria_distance.IN.null, level=0.95)
conf.malaria_distance.IN.null<-exp(confint(malaria_distance.IN.null, level=0.95))
conf.malaria_distance.IN.null

distance_age.IN<-glm(log(distance_in) ~ as.factor(age_class),data = data2_with_distance) # distance modelled by age
distance_age.IN
summary(distance_age.IN)
coef.distance_age.IN<-coef(distance_age.IN)
coef.distance_age.IN
exp.distance_age.IN<- exp(coef(distance_age.IN))
exp.distance_age.IN 
confint(distance_age.IN, level=0.95)
conf.distance_age.IN<-exp(confint(distance_age.IN, level=0.95))
conf.distance_age.IN

distance_occupation.IN<-glm(log(distance_in) ~ data2_with_distance_selected_occupations,data = data2) # distance modelled by occupation
distance_occupation.IN
summary(distance_occupation.IN)
coef.distance_occupation.IN<-coef(distance_occupation.IN)
coef.distance_occupation.IN
exp.distance_occupation.IN<- exp(coef(distance_occupation.IN))
exp.distance_occupation.IN
confint(distance_occupation.IN, level=0.95)
conf.distance_occupation.IN<-exp(confint(distance_occupation.IN, level=0.95))
conf.distance_occupation.IN

##to help understand / interpret results##
####Wald statistic (in univariate case) is compared against a Chi Squared distribution.
####Degrees of freedom are often broadly defined as the number of "observations" (pieces of information) in the data that are free to vary when estimating statistical parameters
####Degres of freedom for a chi squared distribution = number of observations (or pieces of information) minus the number of parameters estimated
####xx check errors (or change / delete)
qchisq(0.90,df=880)
plot(distance_in,fitted.values(malaria_distance.IN.null))
points(distance_in,data2$Malaria_0_1,col="red")

###making predictions###
predict_data<-data.frame(distance=10000)
predict(malaria_distance.IN.null,predict_data,type="response") #probability of malaria being 1

###plotting results - graph of exp(mx+c) against odds. Input m1x+m2X...+c function for any set of predictors. ###
odds_eq<- function(x){exp(( -0.05982*x)-0.76054 )} #for graph of odds - malaria~distance
curve(odds_eq,from=log(1),to=log(150000))
prob_eq<-function(x){exp(( -0.05982*x)-0.76054 )/(1+exp(( -0.05982*x)-0.76054 ))}#for graph of probability - malaria~distance
curve(prob_eq,from=log(1),to=log(150000))

########################

#model TRAVEL OUT#
#######################


##Modelling malaria##

###run glm###

malaria_age.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(age_class), data = data4, family=binomial(link=logit)) # first model, predicting using age class
malaria_age.OUT
summary(malaria_age.OUT)
coef.age.OUT<-coef(malaria_age.OUT)
exp.age.OUT<- exp(coef(malaria_age.OUT))
confint(malaria_age.OUT, level=0.95)
conf.age.OUT<-exp(confint(malaria_age.OUT))

malaria_occupation.OUT<-glm(as.factor(Malaria_0_1) ~ data4_selected_occupations, data = data4, family=binomial(link=logit))
malaria_occupation.OUT
summary(malaria_occupation.OUT)
coef.occupation.OUT<-coef(malaria_occupation.OUT)
exp.occupation.OUT<-exp(coef(malaria_occupation.OUT))
confint(malaria_occupation.OUT, level=0.95)
conf.occupation.OUT<-exp(confint(malaria_occupation.OUT))

#useful
malaria_distance.OUT.null<-glm(as.factor(Malaria_0_1) ~ log(distance_out), data = data4_with_distance, family=binomial(link=logit)) # malaria modelled by distance
malaria_distance.OUT.null 
summary(malaria_distance.OUT.null)
coef.malaria_distance.OUT.null<-coef(malaria_distance.OUT.null)
coef.malaria_distance.OUT.null
exp.malaria_distance.OUT.null<- exp(coef(malaria_distance.OUT.null))
exp.malaria_distance.OUT.null 
confint(malaria_distance.OUT.null, level=0.95)
conf.malaria_distance.OUT.null<-exp(confint(malaria_distance.OUT.null, level=0.95))
conf.malaria_distance.OUT.null

#useful
malaria_time.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(Month.travel), data = data4, family=binomial(link=logit))
malaria_time.OUT
summary(malaria_time.OUT)
coef.malaria_time.OUT<-coef(malaria_time.OUT)
exp.malaria_time.OUT<-exp(coef(malaria_time.OUT))
confint(malaria_time.OUT, level=0.95)
conf.malaria_time.OUT<-exp(confint(malaria_time.OUT))

#useful
time_occupation.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_occupations, data = data4, family=binomial(link=logit))
time_occupation.OUT
summary(time_occupation.OUT)
coef.time_occupation.OUT<-coef(time_occupation.OUT)
exp.time_occupation.OUT<-exp(coef(time_occupation.OUT))
confint(time_occupation.OUT, level=0.95)
conf.time_occupation.OUT<-exp(confint(time_occupation.OUT))

#useful
time_gender.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_genders, data = data4, family=binomial(link=logit))
time_gender.OUT
summary(time_gender.OUT)
coef.time_gender.OUT<-coef(time_gender.OUT)
exp.time_gender.OUT<-exp(coef(time_gender.OUT))
confint(time_gender.OUT, level=0.95)
conf.time_gender.OUT<-exp(confint(time_gender.OUT))

distance_age.OUT<-glm(log(distance_out) ~ as.factor(age_class),data = data4_with_distance) # distance modelled by age
distance_age.OUT  
summary(distance_age.OUT)
coef.distance_age.OUT<-coef(distance_age.OUT)
coef.distance_age.OUT
exp.distance_age.OUT<- exp(coef(distance_age.OUT))
exp.distance_age.OUT 
confint(distance_age.OUT, level=0.95)
conf.distance_age.OUT<-exp(confint(distance_age.OUT, level=0.95))
conf.distance_age.OUT

distance_occupation.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_occupations,data = data4_with_distance) # distance modelled by occupation
distance_occupation.OUT
summary(distance_occupation.OUT)
coef.distance_occupation.OUT<-coef(distance_occupation.OUT)
coef.distance_occupation.OUT
exp.distance_occupation.OUT<- exp(coef(distance_occupation.OUT))
exp.distance_occupation.OUT
confint(distance_occupation.OUT, level=0.95)
conf.distance_occupation.OUT<-exp(confint(distance_occupation.OUT, level=0.95))
conf.distance_occupation.OUT

distance_gender.OUT<-glm(log(distance_out) ~ as.factor(Gender),data = data4_with_distance) # distance modelled by occupation
distance_gender.OUT
summary(distance_gender.OUT)
coef.distance_gender.OUT<-coef(distance_gender.OUT)
coef.distance_gender.OUT
exp.distance_gender.OUT<- exp(coef(distance_gender.OUT))
exp.distance_gender.OUT
confint(distance_gender.OUT, level=0.95)
conf.distance_gender.OUT<-exp(confint(distance_gender.OUT, level=0.95))
conf.distance_gender.OUT

distance_age_occupation.OUT<-glm(log(distance_out) ~ as.factor(age_class)+data4_with_distance_selected_occupations,data = data4_with_distance) # distance modelled by age
distance_age_occupation.OUT  
summary(distance_age_occupation.OUT)
coef.distance_age_occupation.OUT<-coef(distance_age_occupation.OUT)
coef.distance_age_occupation.OUT
exp.distance_age_occupation.OUT<- exp(coef(distance_age_occupation.OUT))
exp.distance_age_occupation.OUT
confint(distance_age_occupation.OUT, level=0.95)
conf.distance_age_occupation.OUT<-exp(confint(distance_age_occupation.OUT, level=0.95))
conf.distance_age_occupation.OUT

malaria_country.OUT<-glm(as.factor(Malaria_0_1) ~ data4_selected_countries,data = data4, family=binomial(link=logit)) # distance modelled by occupation. nb. warnings are valid - variation between 1 and 0 for malaria only exists in Mozambique, Nigeria, South Africa and Zimbabwe
summary(malaria_country.OUT)
coef.malaria_country.OUT<-coef(malaria_country.OUT)
coef.malaria_country.OUT
exp.malaria_country.OUT<- exp(coef(malaria_country.OUT))
exp.malaria_country.OUT
confint(malaria_country.OUT, level=0.95)
conf.malaria_country.OUT<-exp(confint(malaria_country.OUT, level=0.95)) 
conf.malaria_country.OUT
#########xxcheck warnings with Nick. there are 21 cases in countries where all cases from that country were positive. 

malaria_distance_occupation.OUT<-glm(as.factor(Malaria_0_1) ~ log(distance_out)+data4_with_distance_selected_occupations, data = data4_with_distance, family=binomial(link=logit)) 
malaria_distance_occupation.OUT
summary(malaria_distance_occupation.OUT)
coef.malaria_distance_occupation.OUT<-coef(malaria_distance_occupation.OUT)
coef.malaria_distance_occupation.OUT
exp.malaria_distance_occupation.OUT<- exp(coef(malaria_distance_occupation.OUT))
exp.malaria_distance_occupation.OUT 
confint(malaria_distance_occupation.OUT, level=0.95)
conf.malaria_distance_occupation.OUT<-exp(confint(malaria_distance_occupation.OUT, level=0.95))
conf.malaria_distance_occupation.OUT
###################xx check warnings

malaria_travel_prevalence.OUT.null<-glm(as.factor(Malaria_0_1_prevalence_exists) ~ falciparum_travel_prevalence, data = data4_noNA_falciparum_prevalence, family=binomial(link=logit)) # malaria modelled by travel_prevalence
malaria_travel_prevalence.OUT.null 
summary(malaria_travel_prevalence.OUT.null)
coef.malaria_travel_prevalence.OUT.null<-coef(malaria_travel_prevalence.OUT.null)
coef.malaria_travel_prevalence.OUT.null
exp.malaria_travel_prevalence.OUT.null<- exp(coef(malaria_travel_prevalence.OUT.null))
exp.malaria_travel_prevalence.OUT.null 
confint(malaria_travel_prevalence.OUT.null, level=0.95)
conf.malaria_travel_prevalence.OUT.null<-exp(confint(malaria_travel_prevalence.OUT.null, level=0.95))
conf.malaria_travel_prevalence.OUT.null

travel_prevalence_age.OUT<-glm(falciparum_travel_prevalence ~ as.factor(age_class), data = data4_noNA_falciparum_prevalence) 
travel_prevalence_age.OUT
summary(travel_prevalence_age.OUT)
coef.travel_prevalence_age.OUT<-coef(travel_prevalence_age.OUT)
coef.travel_prevalence_age.OUT
exp.travel_prevalence_age.OUT<- exp(coef(travel_prevalence_age.OUT))
exp.travel_prevalence_age.OUT 
confint(travel_prevalence_age.OUT, level=0.95)
conf.travel_prevalence_age.OUT<-exp(confint(travel_prevalence_age.OUT, level=0.95))
conf.travel_prevalence_age.OUT

travel_prevalence_occupation.OUT<-glm(falciparum_travel_prevalence ~ data4_noNA_falciparum_prevalence_selected_occupations, data = data4_noNA_falciparum_prevalence) 
travel_prevalence_occupation.OUT
summary(travel_prevalence_occupation.OUT)
coef.travel_prevalence_occupation.OUT<-coef(travel_prevalence_occupation.OUT)
coef.travel_prevalence_occupation.OUT
exp.travel_prevalence_occupation.OUT<- exp(coef(travel_prevalence_occupation.OUT))
exp.travel_prevalence_occupation.OUT 
confint(travel_prevalence_occupation.OUT, level=0.95)
conf.travel_prevalence_occupation.OUT<-exp(confint(travel_prevalence_occupation.OUT, level=0.95))
conf.travel_prevalence_occupation.OUT

travel_prevalence_gender.OUT<-glm(falciparum_travel_prevalence ~ as.factor(Gender), data = data4_noNA_falciparum_prevalence) 
travel_prevalence_gender.OUT
summary(travel_prevalence_gender.OUT)
coef.travel_prevalence_gender.OUT<-coef(travel_prevalence_gender.OUT)
coef.travel_prevalence_gender.OUT
exp.travel_prevalence_gender.OUT<- exp(coef(travel_prevalence_gender.OUT))
exp.travel_prevalence_gender.OUT 
confint(travel_prevalence_gender.OUT, level=0.95)
conf.travel_prevalence_gender.OUT<-exp(confint(travel_prevalence_gender.OUT, level=0.95))
conf.travel_prevalence_gender.OUT

malaria_Travel_in_out<-glm(as.factor(Malaria_0_1) ~ as.factor(Travel_in_out), data = datain, family=binomial(link=logit)) 
malaria_Travel_in_out
summary(malaria_Travel_in_out)
coef.malaria_Travel_in_out<-coef(malaria_Travel_in_out)
coef.malaria_Travel_in_out
exp.malaria_Travel_in_out<- exp(coef(malaria_Travel_in_out))
exp.malaria_Travel_in_out
confint(malaria_Travel_in_out, level=0.95)
conf.malaria_Travel_in_out<-exp(confint(malaria_Travel_in_out, level=0.95))
conf.malaria_Travel_in_out

###to help understand / interpret results xx check whole section
####for malaria~log(distance)
qchisq(0.90,df=1132)
plot(logdist,fitted.values(malaria_distance.OUT.null))
points(distance,data4$Malaria_0_1,col="red")

###making predictions xx check whole section
####for malaria~log(distance)
############################### xx GIVING TOO MANY PREDICTIONS 
predict_data<-data.frame(log(distance_out)[4])
predict(malaria_distance.OUT.null,predict_data,type="response") #probability of malaria being 1

####malaria~Travel_in_out
newdata<-datain$Travel_in_out 
malaria_Travel_in_out_predict<-predict(malaria_0_1,newdata=0)
plot(malaria_Travel_in_out_predict)

###plotting results 
####for malaria~log(distance)- graph of exp(mx+c) against odds. Input m1x+m2X...+c function for any set of predictors. 
odds_eq<- function(x){exp(( -0.05981622*x)-0.76054374)} #for graph of odds - malaria~distance
curve(odds_eq,from=log(1),to=log(150000))
prob_eq<-function(x){exp(( -0.05981622*x) -0.76054374)/(1+exp((-0.05981622*x) -0.76054374))}#for graph of probability 
curve(prob_eq,from=log(1),to=log(150000))

####distance frequency graph by occupation code 

ggplot() +
  geom_density(data=data4_with_distance_occupation_code1, aes(x=distance_out_occupation1, colour="distance_out_occupation1")) +
  geom_density(data=data4_with_distance_occupation_code2, aes(x=distance_out_occupation2, colour="distance_out_occupation2")) +
  geom_density(data=data4_with_distance_occupation_code3, aes(x=distance_out_occupation3, colour="distance_out_occupation3")) +
  geom_density(data=data4_with_distance_occupation_code4, aes(x=distance_out_occupation4, colour="distance_out_occupation4")) +
  geom_density(data=data4_with_distance_occupation_code5, aes(x=distance_out_occupation5, colour="distance_out_occupation5")) +
  geom_density(data=data4_with_distance_occupation_code6, aes(x=distance_out_occupation6, colour="distance_out_occupation6")) +
  geom_density(data=data4_with_distance_occupation_code8, aes(x=distance_out_occupation8, colour="distance_out_occupation8")) +
  scale_colour_manual("", 
                      breaks = c("distance_out_occupation1", "distance_out_occupation2","distance_out_occupation3","distance_out_occupation4","distance_out_occupation5","distance_out_occupation6","distance_out_occupation7","distance_out_occupation8","distance_out_occupation9","distance_out_occupation10"),
                      values = c("grey", "grey", "grey","blue","green","red","grey")) +
  labs(x="Distance travelled",y="Probability density",fill="Occupation",title = "Distibution of distances travelled outside Swaziland for different occupation groups") +
  theme_classic()

ggsave("distribution of distances travelled outside Swaziland for different occupation groups.pdf")

#outputs#
write.csv(data4_with_distance,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_with_distance.csv")
write.csv(data4_with_distance_occupation_code1,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code1.csv")
write.csv(data4_with_distance_occupation_code2,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code2.csv")
write.csv(data4_with_distance_occupation_code3,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code3.csv")
write.csv(data4_with_distance_occupation_code4,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code4.csv")
write.csv(data4_with_distance_occupation_code5,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code5.csv")
write.csv(data4_with_distance_occupation_code6,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code6.csv")
write.csv(data4_with_distance_occupation_code7,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code7.csv")
write.csv(data4_with_distance_occupation_code8,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code8.csv")
write.csv(data4_with_distance_occupation_code9,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code9.csv")
write.csv(data4_with_distance_occupation_code10,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code10.csv")
write.csv(data4_with_distance_occupation_codeNA,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_codeNA.csv")


