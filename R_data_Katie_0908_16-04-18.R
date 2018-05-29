
# Contents ----------------------------------------------------------------


#Install packages
#Input data
#Define datasets
#Alter variables and create new variables
#Define additional datasets to be used 
#Explore variables 
#Model 

# Install packages  -------------------------------------------------------


#install packages#
install.packages("knitr")
install.packages("dplyr")
install.packages("pander")
install.packages("lme4")
install.packages("stargazer")##for summarising models
install.packages("geosphere")##for calculating distance
install.packages("tidyverse")
install.packages("stargazer")


library(dplyr)
library(knitr)
library(pander)
library(lme4)
library(stargazer)
library(geosphere)
library(tidyverse)


# Call data ---------------------------------------------------------------



datain<-read.csv("/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data.csv", header=T)

##subset the input data ##
###according to DID travel###

data0<-subset(datain,unique.records ==1)



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





###datasets with falciparum_prevalence variable###
####use for all falciparum_prevalence analysis

data4_noNA_falciparum_prevalence<-subset(data4,is.na(falciparum_travel_prevalence)==FALSE)#remove NAs from falciparum prevalence
data4_noNA_falciparum_prevalence_TYPE1or2<-subset(data4_noNA_falciparum_prevalence, TYPE==1|TYPE==2)
length(data4_noNA_falciparum_prevalence$ID)#number of falciparum non-NA values in data4
length((subset(data4,is.na(falciparum_travel_prevalence)==TRUE))$ID)#total number of NAs in data4 falciparum travel prevalence
length(subset(data4,fal_12==-9999)$ID)#number of data4 missing falciparum values due to missing MAP falciparum values 
length(subset(data4,is.na(Year.travel))$ID)#number of data4 missing falciparum values due to missing 'Year.travel' in data
###some falcip values missing from data4 due to a)missing year travel value and b)mising malaria atlas project data.


###occupation variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving occupation_code 
data4_with_distance_selected_occupations<-  factor(data4_with_distance$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data4_noNA_falciparum_prevalence_selected_occupations<- factor(data4_noNA_falciparum_prevalence$Occupation_code, levels=c('1','2','3','4','5','6','8')) 
data4_selected_occupations<-  factor(data4$Occupation_code, levels=c('1','2','3','4','5','6','8','9')) 

###country variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_with_distance_selected_countries<-  factor(data4_with_distance$Country, levels=c( "Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Zambia","Zimbabwe")) 
data4_noNA_falciparum_prevalence_selected_countries<- factor(data4_noNA_falciparum_prevalence$Country, levels=c( "Equitorial Guinea","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Zambia","Zimbabwe")) 
data4_selected_countries<-  factor(data4$Country, levels=c("Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Unknown","Zambia","Zimbabwe")) 


###gender variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_with_distance_selected_genders<-  factor(data4_with_distance$Gender, levels=c("F","M"))
data4_noNA_falciparum_prevalence_selected_genders<-factor(data4_noNA_falciparum_prevalence$Gender, levels=c("F","M"))
data4_selected_genders<-  factor(data4$Gender, levels=c("F","M"))


###reason travel variables###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving reason travel
data4_with_distance_selected_ReasonTravel<-  factor(data4_with_distance$ReasonTravel, levels=c("Business" , "Holiday",  "Other"  ,  "Visiting"))
data4_noNA_falciparum_prevalence_selected_ReasonTravel<-factor(data4_noNA_falciparum_prevalence$ReasonTravel, levels=c("Business" , "Holiday",  "Other"  ,  "Visiting"))
data4_selected_ReasonTravel<-  factor(data4$ReasonTravel, levels=c("Business" , "Holiday",  "Other"  ,  "Visiting"))

###means travel variable###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving means travel
data4_with_distance_selected_MeansTravel<-  factor(data4_with_distance$MeansTravel, levels=c("Personal Car" ,"Airplane","Bicycle","Kombi (van)" , "Large bus"  , "Ride share"  , "Truck"   ,     "Walked" ))
data4_noNA_falciparum_prevalence_selected_MeansTravel<-factor(data4_noNA_falciparum_prevalence$MeansTravel, levels=c("Personal Car" ,"Airplane","Bicycle","Kombi (van)" , "Large bus"  , "Ride share"  , "Truck"   ,     "Walked" ))
data4_selected_MeansTravel<-  factor(data4$MeansTravel, levels=c("Personal Car" ,"Airplane","Bicycle","Kombi (van)" , "Large bus"  , "Ride share"  , "Truck"   ,     "Walked" ))

###Border Post variable###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving border post
data4_with_distance_selected_BorderPost<-  factor(data4_with_distance$BorderPost, levels=c( "Lavumisa/Golela"   ,   "Lomahasha/Namaacha"  , "Mahamba"          ,    "Mananga"       ,       "Matsamo/Jeppe's Reef" ,"Matsapha Airport"  ,  "Mhlumeni/Goba"      ,  "Ngwenya/Oshoek"   ,    "Other"     ,           "Salitje/Onverwacht"  , "Sandlane/Nerston"  ,   "Sicunusa/Houtkop"     ))
data4_noNA_falciparum_prevalence_selected_BorderPost<-factor(data4_noNA_falciparum_prevalence$BorderPost, levels=c("Lavumisa/Golela"   ,   "Lomahasha/Namaacha"  , "Mahamba"          ,    "Mananga"       ,       "Matsamo/Jeppe's Reef" ,"Matsapha Airport"  ,  "Mhlumeni/Goba"      ,  "Ngwenya/Oshoek"   ,    "Other"     ,           "Salitje/Onverwacht"  , "Sandlane/Nerston"  ,   "Sicunusa/Houtkop"      ))
data4_selected_BorderPost<-  factor(data4$BorderPost, levels=c("Lavumisa/Golela"   ,   "Lomahasha/Namaacha"  , "Mahamba"          ,    "Mananga"       ,       "Matsamo/Jeppe's Reef" ,"Matsapha Airport"  ,  "Mhlumeni/Goba"      ,  "Ngwenya/Oshoek"   ,    "Other"     ,           "Salitje/Onverwacht"  , "Sandlane/Nerston"  ,   "Sicunusa/Houtkop"  ))



###manual workers dataset###
data4_manual_workers<- subset(data4,Occupation_code==5)

###students dataset
data4_students<-subset(data4,Occupation_code==8)

###BorderPost datasets
data4_Mhlumeni_Goba<-subset(data4,BorderPost=="Mhlumeni/Goba")
data4_not_Mhlumeni_Goba<-subset(data4,BorderPost!="Mhlumeni/Goba")




# Exploring variables -----------------------------------------------------



##distance_in##
hist(log(distance_in),40,freq=FALSE,	main="Histogram of log(distance_in) to show normal distribution",col="lightcoral");

xseq<-seq(min(log(distance_in)),max(log(distance_in)),0.01)
densities<-dnorm(xseq,mean(log(distance_in)),sd(log(distance_in)))
plot(xseq, densities, col="darkgreen",xlab="Sequence in log(distance_in) range", ylab="Density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'Distance' sample size 
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)

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





# Descriptive stats -------------------------------------------------------



##Overview stats

###subset for descriptive stats
data0_TYPE1<-subset(data0,TYPE==1) 
data0_TYPE2or3<-subset(data0,TYPE==2|TYPE==3) 
data0_TYPE2or3_malariapositive<-subset(data0_TYPE2or3,Malaria_0_1==1)
data4_TYPE_1<-subset(data4,TYPE==1)
data4_TYPE_2<-subset(data4,TYPE==2)
data4_TYPE_3<-subset(data4,TYPE==3)
data4_TYPE2or3<-subset(data4,TYPE==2|TYPE==3)
data4_TYPE_1_malariapositive<-subset(data4_TYPE_1, Malaria_0_1==1)
data4_TYPE_1_malarianegative<-subset(data4_TYPE_1, Malaria_0_1==0)
data4_TYPE2or3_malaria_positive<-subset(data4_TYPE2or3,Malaria_0_1==1)
data4_TYPE2or3_malaria_negative<-subset(data4_TYPE2or3,Malaria_0_1==0)
data4_noNA_falciparum_prevalence_malariapositive<-subset(data4_noNA_falciparum_prevalence, Malaria_0_1==1)
data4_noNA_falciparum_prevalence_malarianegative<-subset(data4_noNA_falciparum_prevalence, Malaria_0_1==0)
data4_yeartravel_2010<-subset(datain, Year.travel==2010)
data4_with_distance_TYPE2or3<- subset(data4_with_distance,TYPE==2|TYPE==3)
data4_noNA_falciparum_prevalence<-subset(data4_noNA_falciparum_prevalence,TYPE==1|TYPE==2)
data4_TYPE_2_malariapositive<-subset(data4_TYPE_2,Malaria_0_1==1)
data4_TYPE_3_malariapositive<-subset(data4_TYPE_3,Malaria_0_1==1)
data4_with_distance_malariapositive<-subset(data4_with_distance,Malaria_0_1==1)
data4_with_distance_malarianegative<-subset(data4_with_distance,Malaria_0_1==0)
data4_malariapositive<-subset(data4,Malaria_0_1==1)
data4_malarianegative<-subset(data4,Malaria_0_1==0)
data4_malariapositive_malariaseason<-subset(data4_malariapositive, Malaria.season==1)
data4_malarianegative_malariaseason<-subset(data4_malarianegative, Malaria.season==1)

mean(data4_with_distance_malariapositive$distance_out)/1000
mean(data4_with_distance_malarianegative$distance_out)/1000
mean(data4_noNA_falciparum_prevalence_malariapositive$falciparum_travel_prevalence)
mean(data4_noNA_falciparum_prevalence_malarianegative$falciparum_travel_prevalence)
length(data4_malariapositive_malariaseason$ID)/length(data4_malariapositive$ID)
length(data4_malarianegative_malariaseason$ID)/length(data4_malarianegative$ID)

stargazer(subset(data4_with_distance[c("Malaria_0_1","distance_out")]), type = "text", title="Descriptive statistics", digits=1, out="test.txt")

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
pie_other_occupations<-barchart_other_occupations_stacked+coord_polar(theta = "y" ) + 
  ylab("Frequency of occupation for cases who \ntravelled outside Swaziland") + 
  xlab("") + 
  labs(fill="'Other' occupations") 




##distance frequencies

###occupation code and distance
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

####
distribution_of_distances_travelled_outside_Swaziland_for_different_occupation_groups<- ggplot() +
  geom_density(data=data4_with_distance_occupation_code1, aes(x=data4_with_distance_occupation_code1$distance_out, colour="data4_with_distance_occupation_code1$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code2, aes(x=data4_with_distance_occupation_code2$distance_out, colour="data4_with_distance_occupation_code2$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code3, aes(x=data4_with_distance_occupation_code3$distance_out, colour="data4_with_distance_occupation_code3$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code4, aes(x=data4_with_distance_occupation_code4$distance_out, colour="data4_with_distance_occupation_code4$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code5, aes(x=data4_with_distance_occupation_code5$distance_out, colour="data4_with_distance_occupation_code5$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code6, aes(x=data4_with_distance_occupation_code6$distance_out, colour="data4_with_distance_occupation_code6$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code7, aes(x=data4_with_distance_occupation_code7$distance_out, colour="data4_with_distance_occupation_code7$distance_out")) +
  geom_density(data=data4_with_distance_occupation_code8, aes(x=data4_with_distance_occupation_code8$distance_out, colour="data4_with_distance_occupation_code8$distance_out")) +
  scale_colour_manual("", 
                      breaks = c("data4_with_distance_occupation_code1$distance_out", "data4_with_distance_occupation_code2$distance_out","data4_with_distance_occupation_code3$distance_out","data4_with_distance_occupation_code4$distance_out","data4_with_distance_occupation_code5$distance_out","data4_with_distance_occupation_code6$distance_out","data4_with_distance_occupation_code7$distance_out","data4_with_distance_occupation_code8$distance_out"),
                      values = c("grey", "grey", "grey","blue","green","red","grey","grey"),
                      labels = c("Unemployed","Farming/Agriculture","Manufacturing/Factory","Office/Clerical Work","Other","Other Manual Labour","Small-market sales or trade","Student")) +
  labs(x="Distance travelled (m)",y="Probability density",fill="Occupation",title = "Continuous distibution of distances travelled outside Swaziland \nfor different occupation groups") +
  theme_classic() +
  theme(legend.position = "bottom") 


###distance and age_class
data4_with_distance_age_class5<-subset(data4_with_distance,age_class==5)
data4_with_distance_age_class_not_5<-subset(data4_with_distance,age_class!=5)
library(ggplot2)
distances_and_age_class_continuous_distribution<-  ggplot()+
  geom_density(data = data4_with_distance_age_class5,aes(x=data4_with_distance_age_class5$distance_out,color=("25<=age<45"))) + 
  geom_density(data=data4_with_distance_age_class_not_5,aes(x=data4_with_distance_age_class_not_5$distance_out,color=("all other ages"))) +
  scale_colour_manual( name="",
                      values = c("red", "grey")) +
  labs(x="Distance travelled (m)",y="Probability density",title = "Continuous distibution of distances travelled outside Swaziland \nfor different age groups") +
  theme_classic() +
  theme(legend.position = "bottom") 


  
###distance and gender
data4_with_distance_male<-subset(data4_with_distance,Gender=="M")
data4_with_distance_female<-subset(data4_with_distance, Gender=="F")
data4_with_distance_gender_NA<-subset(data4_with_distance,Gender="U")
library(ggplot2)
distances_and_gender_continuous_distribution<-   ggplot() +
  geom_density(data=data4_with_distance_male,aes(x=data4_with_distance_male$distance_out,color="Male")) +
  geom_density(data=data4_with_distance_female, aes(x=data4_with_distance_female$distance_out, color="Female")) +
  scale_color_manual(name="",values = c("red","blue")) + 
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor males and females") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and means travel
data4_with_distance_Airplane<-subset(data4_with_distance,MeansTravel=="Airplane")
data4_with_distance_Bicycle<-subset(data4_with_distance,MeansTravel=="Bicycle")
data4_with_distance_Kombi<-subset(data4_with_distance,MeansTravel=="Kombi (van)")
data4_with_distance_Largebus<-subset(data4_with_distance,MeansTravel=="Large bus")
data4_with_distance_PersonalCar<-subset(data4_with_distance,MeansTravel=="Personal Car")
data4_with_distance_Rideshare<-subset(data4_with_distance,MeansTravel=="Ride share")
data4_with_distance_Truck<-subset(data4_with_distance,MeansTravel=="Truck")
data4_with_distance_Walked<-subset(data4_with_distance,MeansTravel=="Walked")
data4_with_distance_meanstravel_NA<-subset(data4_with_distance,MeansTravel=="")
library(ggplot2)
distance_and_meanstravel_continuous_distribution<- ggplot() +
  geom_density(data=data4_with_distance_Airplane,aes(x=data4_with_distance_Airplane$distance_out,color="Airplane")) +
  geom_density(data=data4_with_distance_Kombi,aes(x=data4_with_distance_Kombi$distance_out,color="Kombi (van)")) +
  geom_density(data=data4_with_distance_Largebus,aes(x=data4_with_distance_Largebus$distance_out,color="Large Bus")) +
  geom_density(data=data4_with_distance_PersonalCar,aes(x=data4_with_distance_PersonalCar$distance_out,color="Personal Car")) +
  geom_density(data=data4_with_distance_Rideshare,aes(x=data4_with_distance_Rideshare$distance_out,color="Ride Share")) +
  geom_density(data=data4_with_distance_Walked,aes(x=data4_with_distance_Walked$distance_out,color="Walked")) +
  geom_density(data=data4_with_distance_NA,aes(x=data4_with_distance_NA$distance_out,color="NA")) +
  
  scale_color_manual(name="", values=c("grey","blue","yellow","brown","violet","red","orange","pink","green")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different modes of transport") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and border posts
data4_with_distance_LavumisaGolela<-subset(data4_with_distance,BorderPost=="Lavumisa/Golela")
data4_with_distance_LomahashaNamaacha<-subset(data4_with_distance,BorderPost=="Lomahasha/Namaacha")
data4_with_distance_Mahamba<-subset(data4_with_distance,BorderPost=="Mahamba")
data4_with_distance_Mananga<-subset(data4_with_distance,BorderPost=="Mananga")
data4_with_distance_MatsamoJeppesReef<-subset(data4_with_distance,BorderPost=="Matsamo/Jeppe\'s Reef")
data4_with_distance_MatsaphaAirport<-subset(data4_with_distance,BorderPost=="Matsapha Airport")
data4_with_distance_MhlumeniGoba<-subset(data4_with_distance,BorderPost=="Mhlumeni/Goba")
data4_with_distance_NgwenyaOshoek<-subset(data4_with_distance,BorderPost=="Ngwenya/Oshoek")
data4_with_distance_SalitjeOnverwacht<-subset(data4_with_distance,BorderPost=="Salitje/Onverwacht")
data4_with_distance_SandlaneNerston<-subset(data4_with_distance,BorderPost=="Sandlane/Nerston")
data4_with_distance_SicunusaHoutkop<-subset(data4_with_distance,BorderPost=="Sicunusa/Houtkop")

library(ggplot2)
distance_and_borderpost_continuous_distribution<- ggplot() +
  geom_density(data=data4_with_distance_LavumisaGolela,aes(x=data4_with_distance_LavumisaGolela$distance_out,color="Lavumisa/Golela")) +
  geom_density(data=data4_with_distance_LomahashaNamaacha,aes(x=data4_with_distance_LomahashaNamaacha$distance_out,color="Lomahasha/Namaacha")) +
  geom_density(data=data4_with_distance_Mahamba,aes(x=data4_with_distance_Mahamba$distance_out,color="Mahamba")) +
  geom_density(data=data4_with_distance_Mananga,aes(x=data4_with_distance_Mananga$distance_out,color="Manangar")) +
  geom_density(data=data4_with_distance_MatsamoJeppesReef,aes(x=data4_with_distance_MatsamoJeppesReef$distance_out,color="Matsamo/Jeppe\'s Reef")) +
  geom_density(data=data4_with_distance_MatsaphaAirport,aes(x=data4_with_distance_MatsaphaAirport$distance_out,color="Matsapha Airport")) +
  geom_density(data=data4_with_distance_MhlumeniGoba,aes(x=data4_with_distance_MhlumeniGoba$distance_out,color="Mhlumeni/Goba")) +
  geom_density(data=data4_with_distance_NgwenyaOshoek,aes(x=data4_with_distance_NgwenyaOshoek$distance_out,color="Ngwenya/Oshoek")) +
  geom_density(data=data4_with_distance_SalitjeOnverwacht,aes(x=data4_with_distance_SalitjeOnverwacht$distance_out,color="Salitje/Onverwacht")) +
  geom_density(data=data4_with_distance_SandlaneNerston,aes(x=data4_with_distance_SandlaneNerston$distance_out,color="Sandlane/Nerston")) +
  geom_density(data=data4_with_distance_SicunusaHoutkop,aes(x=data4_with_distance_SicunusaHoutkop$distance_out,color="Sicunusa/Houtkop")) +
  
  scale_color_manual(name="", values=c("grey","blue","yellow","brown","black","red","orange","grey","green","beige","darkred")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different border crossings") +
  theme_classic() +
  theme(legend.position = "bottom")



###distance and pop home
library(ggplot2)
  pop.HOME_vs_distance_scatter<- ggplot(data=data4_with_distance,aes(x=data4_with_distance$pop.HOME,y=data4_with_distance$distance_out)) +
  geom_point() +
  labs(x="Population of home locality", y="Distance travelled (m)", title="Relationship between home population and distance travelled") +
    theme_classic() +
    theme(legend.position = "bottom")
  
###distance and Home urban_rural distance
library(ggplot2)
HOME_rural_urban_distance_vs_distance_scatter<- ggplot(data=data4_with_distance,aes(x=data4_with_distance$HOME_urban_rural_distance..degrees.,y=data4_with_distance$distance_out)) +
  geom_point() +
  labs(x="Distance to nearest urban centre (degrees)", y="Distance travelled (m)", title="Relationship between distance of home from nearest urban centre \nand distance travelled") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and month travel
data4_with_distance_jan<-subset(data4_with_distance,Month.travel=="1")
data4_with_distance_feb<-subset(data4_with_distance,Month.travel=="2")
data4_with_distance_mar<-subset(data4_with_distance,Month.travel=="3")
data4_with_distance_apr<-subset(data4_with_distance,Month.travel=="4")
data4_with_distance_may<-subset(data4_with_distance,Month.travel=="5")
data4_with_distance_jun<-subset(data4_with_distance,Month.travel=="6")
data4_with_distance_jul<-subset(data4_with_distance,Month.travel=="7")
data4_with_distance_aug<-subset(data4_with_distance,Month.travel=="8")
data4_with_distance_sep<-subset(data4_with_distance,Month.travel=="9")
data4_with_distance_oct<-subset(data4_with_distance,Month.travel=="10")
data4_with_distance_nov<-subset(data4_with_distance,Month.travel=="11")
data4_with_distance_dec<-subset(data4_with_distance,Month.travel=="12")

library(ggplot2)
distance_and_monthtravel_continuous_distribution<- ggplot() +
  geom_density(data=data4_with_distance_jan,aes(x=data4_with_distance_jan$distance_out,color="Jan")) +
  geom_density(data=data4_with_distance_feb,aes(x=data4_with_distance_feb$distance_out,color="Feb")) +
  geom_density(data=data4_with_distance_mar,aes(x=data4_with_distance_mar$distance_out,color="Mar")) +
  geom_density(data=data4_with_distance_apr,aes(x=data4_with_distance_apr$distance_out,color="Apr")) +
  geom_density(data=data4_with_distance_may,aes(x=data4_with_distance_may$distance_out,color="May")) +
  geom_density(data=data4_with_distance_jun,aes(x=data4_with_distance_jun$distance_out,color="Jun")) +
  geom_density(data=data4_with_distance_jul,aes(x=data4_with_distance_jul$distance_out,color="Jul")) +
  geom_density(data=data4_with_distance_aug,aes(x=data4_with_distance_aug$distance_out,color="Aug")) +
  geom_density(data=data4_with_distance_sep,aes(x=data4_with_distance_sep$distance_out,color="Sep")) +
  geom_density(data=data4_with_distance_oct,aes(x=data4_with_distance_oct$distance_out,color="Oct")) +
  geom_density(data=data4_with_distance_nov,aes(x=data4_with_distance_nov$distance_out,color="Nov")) +
  geom_density(data=data4_with_distance_dec,aes(x=data4_with_distance_dec$distance_out,color="Dec")) +
  
  scale_color_manual(name="", breaks=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),values=c("grey","blue","yellow","brown","black","red","orange","grey","green","beige","darkred","lightblue")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different months, 2012-2014") +
  theme_classic() +
  theme(legend.position = "bottom")




##time frequencies
###number of students who travel in each month
library(ggplot2)
student_seasonal_travel_frequency<- ggplot(data=data4_students, aes(as.factor(data4_students$Month.travel), fill = as.factor(data4_students$High_risk_month_or_malaria_season))) +
  geom_bar() + 
  scale_fill_manual("legend",values = c("red", "yellow","orange","blue"), labels=c("Both", "High risk month\n according \nto Swaziland data","Malaria season in\n southern Africa", "Neither")) +
  labs(title = "Histogram showing frequency of \nstudent travel in different months, 2012-2014", caption = "Source: xx") +
  xlab("Month of travel") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
student_seasonal_travel_frequency 


##number of people using different transport types in each month 
library(ggplot2)

month_vs_means_travel_frequency<-ggplot(data = data4, aes(as.factor(data4$Month.travel))) +
  geom_bar(aes(fill=data4$MeansTravel)) + 
  theme_classic() +
  theme (legend.position = "right") +
  xlab( "Month of travel") +
  ylab ("Frequency") +
  labs(title="Modes of transport for travel outside of Swaziland across \ndifferent months, 2012-2014",caption = "Source: xx") +
  guides(fill=guide_legend(title=NULL,label=TRUE)) +
  scale_fill_manual(values=c("grey","blue","yellow","brown","violet","red","orange","pink","green"),name="Mode of travel", breaks=c("","Airplane","Bicycle", "Kombi (van)" , "Large bus"   , "Personal Car" ,"Ride share" ,  "Truck"   ,     "Walked"      ), labels=c("NA","Airplane","Bicycle", "Kombi (van)" , "Large bus"   , "Personal Car" ,"Ride share" ,  "Truck"   ,     "Walked"    )) + 
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
month_vs_means_travel_frequency
  
##Number of people travelling through different border posts in high risk and low risk months
library(ggplot2)

High_risk_month_vs_Mhlumeni_Goba_travel_frequency<-ggplot(data=data4_Mhlumeni_Goba, aes(data4_Mhlumeni_Goba$High.risk.month)) +
  geom_bar() +
  labs(title="Frequency of travel out of Mhlumeni/Goba border\n point during high risk and non-high risk months, 2012-2014",caption = "Source: xx") +
  scale_fill_manual(values=c("red")) +
  ylab ("Frequency") +
  scale_x_continuous(name= "",position = "bottom", breaks=c(0,1),labels = c("Travel during non-high risk month", "Travel during high risk month"))
High_risk_month_vs_non_Mhlumeni_Goba_travel_frequency<-ggplot(data=data4_not_Mhlumeni_Goba, aes(data4_not_Mhlumeni_Goba$High.risk.month)) +
  geom_bar() +
  labs(title="Frequency of travel out of Mhlumeni/Goba border point\n during high risk and non-high risk months, 2012-2014",caption = "Source: xx") +
  scale_fill_manual(values=c("red")) +
  ylab ("Frequency") +
  scale_x_continuous(name= "",position = "bottom", breaks=c(0,1),labels = c("Travel during non-high risk month", "Travel during high risk month"))

##Urban rural distance for people travelling in different months
library(ggplot2)

month_vs_home_urban_rural_distance_scatter<- ggplot(data=data4,aes(x=as.factor(data4$Month.travel),y=data4$HOME_urban_rural_distance..degrees., col=as.factor(data4$High_risk_month_or_malaria_season))) +
  geom_point() +
  xlab("Month of travel") +
  ylab ("Home distance from urban area (degrees)") +
  labs(title="Relationship between month of travel and distance \nbetween home location and nearest urban centre", caption="Source: xx") +
  scale_x_discrete(labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  
  scale_color_manual("legend",values = c("grey","red", "yellow","orange","blue"), labels=c("NA","Both", "High risk month\n according \nto Swaziland data","Malaria season in\n southern Africa", "Neither")) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")




# Model TRAVEL OUT --------------------------------------------------------




##Modelling malaria##

###run simple linear and logistic univariate glms###
####Things affecting malaria - data4 TYPE ==1 or TYPE==2 (reactively detected only )

malaria_distance.OUT<-glm(as.factor(Malaria_0_1) ~ log(distance_out), data = data4_with_distance_TYPE2or3, family=binomial(link=logit)) # malaria modelled by distance
malaria_distance.OUT 
summary(malaria_distance.OUT)
coef.malaria_distance.OUT<-coef(malaria_distance.OUT)
coef.malaria_distance.OUT
exp.malaria_distance.OUT<- exp(coef(malaria_distance.OUT))
exp.malaria_distance.OUT 
confint(malaria_distance.OUT, level=0.95)
conf.malaria_distance.OUT<-exp(confint(malaria_distance.OUT, level=0.95))
conf.malaria_distance.OUT

malaria_travel_prevalence.OUT<-glm(as.factor(Malaria_0_1_prevalence_exists) ~ data4_noNA_falciparum_prevalence_TYPE1or2, data = data4_noNA_falciparum_prevalence, family=binomial(link=logit)) # malaria modelled by travel_prevalence
malaria_travel_prevalence.OUT 
summary(malaria_travel_prevalence.OUT)
coef.malaria_travel_prevalence.OUT<-coef(malaria_travel_prevalence.OUT)
coef.malaria_travel_prevalence.OUT
exp.malaria_travel_prevalence.OUT<- exp(coef(malaria_travel_prevalence.OUT))
exp.malaria_travel_prevalence.OUT 
confint(malaria_travel_prevalence.OUT, level=0.95)
conf.malaria_travel_prevalence.OUT<-exp(confint(malaria_travel_prevalence.OUT, level=0.95))
conf.malaria_travel_prevalence.OUT

malaria_time.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(Month.travel), data = data4_TYPE2or3, family=binomial(link=logit))
malaria_time.OUT
summary(malaria_time.OUT)
coef.malaria_time.OUT<-coef(malaria_time.OUT)
exp.malaria_time.OUT<-exp(coef(malaria_time.OUT))
confint(malaria_time.OUT, level=0.95)
conf.malaria_time.OUT<-exp(confint(malaria_time.OUT))

malaria_country.OUT<-glm(as.factor(Malaria_0_1) ~ data4_selected_countries,data = data4_TYPE2or3, family=binomial(link=logit)) # distance modelled by occupation. nb. warnings are valid - variation between 1 and 0 for malaria only exists in Mozambique, Nigeria, South Africa and Zimbabwe
summary(malaria_country.OUT)
coef.malaria_country.OUT<-coef(malaria_country.OUT)
coef.malaria_country.OUT
exp.malaria_country.OUT<- exp(coef(malaria_country.OUT))
exp.malaria_country.OUT
confint(malaria_country.OUT, level=0.95)
conf.malaria_country.OUT<-exp(confint(malaria_country.OUT, level=0.95)) 
conf.malaria_country.OUT


####Things affecting malaria - all data4 (TYPE==1 or TYPE==2 or TYPE==3)

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

malaria_distance.OUT<-glm(as.factor(Malaria_0_1) ~ log(distance_out), data = data4_with_distance, family=binomial(link=logit)) # malaria modelled by distance
malaria_distance.OUT 
summary(malaria_distance.OUT)
coef.malaria_distance.OUT<-coef(malaria_distance.OUT)
coef.malaria_distance.OUT
exp.malaria_distance.OUT<- exp(coef(malaria_distance.OUT))
exp.malaria_distance.OUT 
confint(malaria_distance.OUT, level=0.95)
conf.malaria_distance.OUT<-exp(confint(malaria_distance.OUT, level=0.95))
conf.malaria_distance.OUT

malaria_travel_prevalence.OUT<-glm(as.factor(Malaria_0_1_prevalence_exists) ~ falciparum_travel_prevalence, data = data4_noNA_falciparum_prevalence, family=binomial(link=logit)) # malaria modelled by travel_prevalence
malaria_travel_prevalence.OUT 
summary(malaria_travel_prevalence.OUT)
coef.malaria_travel_prevalence.OUT<-coef(malaria_travel_prevalence.OUT)
coef.malaria_travel_prevalence.OUT
exp.malaria_travel_prevalence.OUT<- exp(coef(malaria_travel_prevalence.OUT))
exp.malaria_travel_prevalence.OUT 
confint(malaria_travel_prevalence.OUT, level=0.95)
conf.malaria_travel_prevalence.OUT<-exp(confint(malaria_travel_prevalence.OUT, level=0.95))
conf.malaria_travel_prevalence.OUT

malaria_time.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(Month.travel), data = data4, family=binomial(link=logit))
malaria_time.OUT
summary(malaria_time.OUT)
coef.malaria_time.OUT<-coef(malaria_time.OUT)
exp.malaria_time.OUT<-exp(coef(malaria_time.OUT))
confint(malaria_time.OUT, level=0.95)
conf.malaria_time.OUT<-exp(confint(malaria_time.OUT))

malaria_country.OUT<-glm(as.factor(Malaria_0_1) ~ data4_selected_countries,data = data4, family=binomial(link=logit)) # distance modelled by occupation. nb. warnings are valid - variation between 1 and 0 for malaria only exists in Mozambique, Nigeria, South Africa and Zimbabwe
summary(malaria_country.OUT)
coef.malaria_country.OUT<-coef(malaria_country.OUT)
coef.malaria_country.OUT
exp.malaria_country.OUT<- exp(coef(malaria_country.OUT))
exp.malaria_country.OUT
confint(malaria_country.OUT, level=0.95)
conf.malaria_country.OUT<-exp(confint(malaria_country.OUT, level=0.95)) 
conf.malaria_country.OUT
######### xx check warnings with Nick. there are 21 cases in countries where all cases from that country were positive. 

#####Things affecting things affecting malaria

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

distance_gender.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_genders ,data = data4_with_distance) # distance modelled by occupation
distance_gender.OUT
summary(distance_gender.OUT)
coef.distance_gender.OUT<-coef(distance_gender.OUT)
coef.distance_gender.OUT
exp.distance_gender.OUT<- exp(coef(distance_gender.OUT))
exp.distance_gender.OUT
confint(distance_gender.OUT, level=0.95)
conf.distance_gender.OUT<-exp(confint(distance_gender.OUT, level=0.95))
conf.distance_gender.OUT

distance_reason_travel.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_ReasonTravel,data = data4_with_distance) # distance modelled by occupation
distance_reason_travel.OUT
summary(distance_reason_travel.OUT)
coef.distance_reason_travel.OUT<-coef(distance_reason_travel.OUT)
coef.distance_reason_travel.OUT
exp.distance_reason_travel.OUT<- exp(coef(distance_reason_travel.OUT))
exp.distance_reason_travel.OUT
confint(distance_reason_travel.OUT, level=0.95)
conf.distance_reason_travel.OUT<-exp(confint(distance_reason_travel.OUT, level=0.95))
conf.distance_reason_travel.OUT

distance_means_travel.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_MeansTravel,data = data4_with_distance) # distance modelled by occupation
distance_means_travel.OUT
summary(distance_means_travel.OUT)
coef.distance_means_travel.OUT<-coef(distance_means_travel.OUT)
coef.distance_means_travel.OUT
exp.distance_means_travel.OUT<- exp(coef(distance_means_travel.OUT))
exp.distance_means_travel.OUT
confint(distance_means_travel.OUT, level=0.95)
conf.distance_means_travel.OUT<-exp(confint(distance_means_travel.OUT, level=0.95))
conf.distance_means_travel.OUT
 
distance_border_post.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_BorderPost,data = data4_with_distance) # distance modelled by occupation
distance_border_post.OUT
summary(distance_border_post.OUT)
coef.distance_border_post.OUT<-coef(distance_border_post.OUT)
coef.distance_border_post.OUT
exp.distance_border_post.OUT<- exp(coef(distance_border_post.OUT))
exp.distance_border_post.OUT
confint(distance_border_post.OUT, level=0.95)
conf.distance_border_post.OUT<-exp(confint(distance_border_post.OUT, level=0.95))
conf.distance_border_post.OUT

distance_pop_HOME.OUT<-glm(log(distance_out) ~ data4_with_distance$pop.HOME,data = data4_with_distance) # distance modelled by occupation
distance_pop_HOME.OUT
summary(distance_pop_HOME.OUT)
coef.distance_pop_HOME.OUT<-coef(distance_pop_HOME.OUT)
coef.distance_pop_HOME.OUT
exp.distance_pop_HOME.OUT<- exp(coef(distance_pop_HOME.OUT))
exp.distance_pop_HOME.OUT
confint(distance_pop_HOME.OUT, level=0.95)
conf.distance_pop_HOME.OUT<-exp(confint(distance_pop_HOME.OUT, level=0.95))
conf.distance_pop_HOME.OUT

distance_HOME_urban_rural_distance.OUT<-glm(log(distance_out) ~ data4_with_distance$HOME_urban_rural_distance..degrees.,data = data4_with_distance) # distance modelled by occupation
distance_HOME_urban_rural_distance.OUT
summary(distance_HOME_urban_rural_distance.OUT)
coef.distance_HOME_urban_rural_distance.OUT<-coef(distance_HOME_urban_rural_distance.OUT)
coef.distance_HOME_urban_rural_distance.OUT
exp.distance_HOME_urban_rural_distance.OUT<- exp(coef(distance_HOME_urban_rural_distance.OUT))
exp.distance_HOME_urban_rural_distance.OUT
confint(distance_HOME_urban_rural_distance.OUT, level=0.95)
conf.distance_HOME_urban_rural_distance.OUT<-exp(confint(distance_HOME_urban_rural_distance.OUT, level=0.95))
conf.distance_HOME_urban_rural_distance.OUT

distance_time.OUT<-glm(log(distance_out) ~ as.factor(Month.travel),data = data4_with_distance) # distance modelled by age
distance_time.OUT
summary(distance_time.OUT)
coef.distance_time.OUT<-coef(distance_time.OUT)
coef.distance_time.OUT
exp.distance_time.OUT<- exp(coef(distance_time.OUT))
exp.distance_time.OUT
confint(distance_time.OUT, level=0.95)
conf.distance_time.OUT<-exp(confint(distance_time.OUT, level=0.95))
conf.distance_time.OUT






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

travel_prevalence_ReasonTravel.OUT<-glm(falciparum_travel_prevalence ~ data4_noNA_falciparum_prevalence_selected_ReasonTravel, data = data4_noNA_falciparum_prevalence) 
travel_prevalence_ReasonTravel.OUT
summary(travel_prevalence_ReasonTravel.OUT)
coef.travel_prevalence_ReasonTravel.OUT<-coef(travel_prevalence_ReasonTravel.OUT)
coef.travel_prevalence_ReasonTravel.OUT
exp.travel_prevalence_ReasonTravel.OUT<- exp(coef(travel_prevalence_ReasonTravel.OUT))
exp.travel_prevalence_ReasonTravel.OUT
confint(travel_prevalence_ReasonTravel.OUT, level=0.95)
conf.travel_prevalence_ReasonTravel.OUT<-exp(confint(travel_prevalence_ReasonTravel.OUT, level=0.95))
conf.travel_prevalence_ReasonTravel.OUT

travel_prevalence_MeansTravel.OUT<-glm(falciparum_travel_prevalence ~ data4_noNA_falciparum_prevalence_selected_MeansTravel, data = data4_noNA_falciparum_prevalence) 
travel_prevalence_MeansTravel.OUT
summary(travel_prevalence_MeansTravel.OUT)
coef.travel_prevalence_MeansTravel.OUT<-coef(travel_prevalence_MeansTravel.OUT)
coef.travel_prevalence_MeansTravel.OUT
exp.travel_prevalence_MeansTravel.OUT<- exp(coef(travel_prevalence_MeansTravel.OUT))
exp.travel_prevalence_MeansTravel.OUT
confint(travel_prevalence_MeansTravel.OUT, level=0.95)
conf.travel_prevalence_MeansTravel.OUT<-exp(confint(travel_prevalence_MeansTravel.OUT, level=0.95))
conf.travel_prevalence_MeansTravel.OUT

travel_prevalence_BorderPost.OUT<-glm(falciparum_travel_prevalence ~ data4_noNA_falciparum_prevalence_selected_BorderPost, data = data4_noNA_falciparum_prevalence) 
travel_prevalence_BorderPost.OUT
summary(travel_prevalence_BorderPost.OUT)
coef.travel_prevalence_BorderPost.OUT<-coef(travel_prevalence_BorderPost.OUT)
coef.travel_prevalence_BorderPost.OUT
exp.travel_prevalence_BorderPost.OUT<- exp(coef(travel_prevalence_BorderPost.OUT))
exp.travel_prevalence_BorderPost.OUT
confint(travel_prevalence_BorderPost.OUT, level=0.95)
conf.travel_prevalence_BorderPost.OUT<-exp(confint(travel_prevalence_BorderPost.OUT, level=0.95))
conf.travel_prevalence_BorderPost.OUT

travel_prevalence_pop.HOME.OUT<-glm(falciparum_travel_prevalence ~ pop.HOME, data = data4_noNA_falciparum_prevalence) 
travel_prevalence_pop.HOME.OUT
summary(travel_prevalence_pop.HOME.OUT)
coef.travel_prevalence_pop.HOME.OUT<-coef(travel_prevalence_pop.HOME.OUT)
coef.travel_prevalence_pop.HOME.OUT
exp.travel_prevalence_pop.HOME.OUT<- exp(coef(travel_prevalence_pop.HOME.OUT))
exp.travel_prevalence_pop.HOME.OUT
confint(travel_prevalence_pop.HOME.OUT, level=0.95)
conf.travel_prevalence_pop.HOME.OUT<-exp(confint(travel_prevalence_pop.HOME.OUT, level=0.95))
conf.travel_prevalence_pop.HOME.OUT

travel_prevalence_pop.HOME_urban_rural_distance.OUT<-glm(falciparum_travel_prevalence ~ HOME_urban_rural_distance..degrees., data = data4_noNA_falciparum_prevalence) 
travel_prevalence_pop.HOME_urban_rural_distance.OUT
summary(travel_prevalence_pop.HOME_urban_rural_distance.OUT)
coef.travel_prevalence_pop.HOME_urban_rural_distance.OUT<-coef(travel_prevalence_pop.HOME_urban_rural_distance.OUT)
coef.travel_prevalence_pop.HOME_urban_rural_distance.OUT
exp.travel_prevalence_pop.HOME_urban_rural_distance.OUT<- exp(coef(travel_prevalence_pop.HOME_urban_rural_distance.OUT))
exp.travel_prevalence_pop.HOME_urban_rural_distance.OUT
confint(travel_prevalence_pop.HOME_urban_rural_distance.OUT, level=0.95)
conf.travel_prevalence_pop.HOME_urban_rural_distance.OUT<-exp(confint(travel_prevalence_pop.HOME_urban_rural_distance.OUT, level=0.95))
conf.travel_prevalence_pop.HOME_urban_rural_distance.OUT





time_age.OUT<-glm(as.factor(High.risk.month) ~ as.factor(age_class), data = data4, family=binomial(link=logit))
time_age.OUT
summary(time_age.OUT)
coef.time_age.OUT<-coef(time_age.OUT)
exp.time_age.OUT<-exp(coef(time_age.OUT))
confint(time_age.OUT, level=0.95)
conf.time_age.OUT<-exp(confint(time_age.OUT))

time_occupation.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_occupations, data = data4, family=binomial(link=logit))
time_occupation.OUT
summary(time_occupation.OUT)
coef.time_occupation.OUT<-coef(time_occupation.OUT)
exp.time_occupation.OUT<-exp(coef(time_occupation.OUT))
confint(time_occupation.OUT, level=0.95)
conf.time_occupation.OUT<-exp(confint(time_occupation.OUT)) 

time_gender.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_genders, data = data4, family=binomial(link=logit))
time_gender.OUT
summary(time_gender.OUT)
coef.time_gender.OUT<-coef(time_gender.OUT)
exp.time_gender.OUT<-exp(coef(time_gender.OUT))
confint(time_gender.OUT, level=0.95)
conf.time_gender.OUT<-exp(confint(time_gender.OUT))

time_reason_travel.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_ReasonTravel, data = data4, family=binomial(link=logit))
time_reason_travel.OUT 
summary(time_reason_travel.OUT) 
coef.time_reason_travel.OUT<-coef(time_reason_travel.OUT) 
exp.time_reason_travel.OUT<-exp(coef(time_reason_travel.OUT)) 
confint(time_reason_travel.OUT, level=0.95) 
conf.time_reason_travel.OUT<-exp(confint(time_reason_travel.OUT)) 

time_means_travel.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_MeansTravel, data = data4, family=binomial(link=logit))
time_means_travel.OUT
summary(time_means_travel.OUT) 
coef.time_means_travel.OUT<-coef(time_means_travel.OUT) 
exp.time_means_travel.OUT<-exp(coef(time_means_travel.OUT)) 
confint(time_means_travel.OUT, level=0.95) 
conf.time_means_travel.OUT<-exp(confint(time_means_travel.OUT)) 

time_border_post.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_BorderPost, data = data4, family=binomial(link=logit))
time_border_post.OUT
summary(time_border_post.OUT) 
coef.time_border_post.OUT<-coef(time_border_post.OUT) 
exp.time_border_post.OUT<-exp(coef(time_border_post.OUT)) 
confint(time_border_post.OUT, level=0.95) 
conf.time_border_post.OUT<-exp(confint(time_border_post.OUT)) 

time_pop_HOME.OUT<-glm(as.factor(High.risk.month) ~ pop.HOME, data = data4, family=binomial(link=logit))
time_pop_HOME.OUT
summary(time_pop_HOME.OUT) 
coef.time_pop_HOME.OUT<-coef(time_pop_HOME.OUT) 
exp.time_pop_HOME.OUT<-exp(coef(time_pop_HOME.OUT)) 
confint(time_pop_HOME.OUT, level=0.95) 
conf.time_pop_HOME.OUT<-exp(confint(time_pop_HOME.OUT)) 

time_pop_urban_rural_distance.OUT<-glm(as.factor(High.risk.month) ~ HOME_urban_rural_distance..degrees., data = data4, family=binomial(link=logit))
time_pop_urban_rural_distance.OUT
summary(time_pop_urban_rural_distance.OUT) 
coef.time_pop_urban_rural_distance.OUT<-coef(time_pop_urban_rural_distance.OUT) 
exp.time_pop_urban_rural_distance.OUT<-exp(coef(time_pop_urban_rural_distance.OUT)) 
confint(time_pop_urban_rural_distance.OUT, level=0.95) 
conf.time_pop_urban_rural_distance.OUT<-exp(confint(time_pop_urban_rural_distance.OUT)) 

###Multiple linear and logistic regression univariate models

distance.OUT<-glm(log(distance_out) ~ data4_with_distance_selected_occupations + as.factor(age_class) + data4_with_distance_selected_genders + data4_with_distance_selected_ReasonTravel +data4_with_distance_selected_MeansTravel + data4_with_distance_selected_BorderPost + pop.HOME + HOME_urban_rural_distance..degrees. + Month.travel, data=data4_with_distance )
distance.OUT
summary(distance.OUT) 
coef.distance.OUT<-coef(distance.OUT) 
exp.distance.OUT<-exp(coef(distance.OUT)) 
confint(distance.OUT, level=0.95) 
conf.distance.OUT<-exp(confint(distance.OUT)) 

travel_prevalence.OUT<- glm(falciparum_travel_prevalence ~ data4_noNA_falciparum_prevalence_selected_occupations + data4_noNA_falciparum_prevalence_selected_genders + data4_noNA_falciparum_prevalence_selected_ReasonTravel + data4_noNA_falciparum_prevalence_selected_BorderPost + pop.HOME + HOME_urban_rural_distance..degrees. , data=data4_noNA_falciparum_prevalence)
travel_prevalence.OUT
summary(travel_prevalence.OUT) 
coef.travel_prevalence.OUT<-coef(travel_prevalence.OUT) 
exp.travel_prevalence.OUT<-exp(coef(travel_prevalence.OUT)) 
confint(travel_prevalence.OUT, level=0.95) 
conf.travel_prevalence.OUT<-exp(confint(travel_prevalence.OUT)) 

time.OUT<- glm(as.factor(High.risk.month) ~ data4_selected_occupations + data4_selected_ReasonTravel + data4_selected_MeansTravel + data4_selected_BorderPost + HOME_urban_rural_distance..degrees., data=data4, family=binomial(link=logit))
time.OUT
summary(time.OUT) 
coef.time.OUT<-coef(time.OUT) 
exp.time.OUT<-exp(coef(time.OUT)) 
confint(time.OUT, level=0.95) 
conf.time.OUT<-exp(confint(time.OUT)) 

###Multiple regression multivariate model



###displaying results in table
library(stargazer)

stargazer (malaria_Travel_in_out, malaria_distance.OUT, malaria_travel_prevalence.OUT, malaria_time.OUT, malaria_country.OUT,  type = "html", title = "Coefficient values and significance of variables for univariate logistic models of variables affecting malaria incidence", style = "default", 
           summary = NULL, out = "regression_table_malaria.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Malaria incidence models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) malaria ~ international or domestic travel, (2) malaria ~ distance, (3) malaria ~ P. falcip prevalence, (4) malaria ~ time of year (5) malaria ~ country", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)

stargazer (distance_age.OUT,distance_occupation.OUT,distance_gender.OUT, distance_reason_travel.OUT, distance_means_travel.OUT, distance_border_post.OUT, distance_pop_HOME.OUT, distance_HOME_urban_rural_distance.OUT,distance_time.OUT,   type = "html", title = "Coefficient values and significance of variables for univariate logistic models of variables affecting distance", style = "default", 
           summary = NULL, out = "regression_table_distance.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Distance models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) distance ~ age, (2) distance ~ occupation, (3) distance ~ gender, (4) distance ~ reason for travel (5) distance ~ travel means (6) distance ~ border post (7) distance ~ home population (8) distance ~ home distance to nearest urban centre (9) distance ~ travel time of year", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)

stargazer (travel_prevalence_age.OUT,travel_prevalence_occupation.OUT,travel_prevalence_gender.OUT, travel_prevalence_ReasonTravel.OUT, travel_prevalence_MeansTravel.OUT, 
           travel_prevalence_BorderPost.OUT, travel_prevalence_pop.HOME.OUT, travel_prevalence_pop.HOME_urban_rural_distance.OUT,   type = "html", 
           title = "Coefficient values and significance of variables for univariate logistic models of variables affecting P. falcip. prevalence at travel location", style = "default", 
           summary = NULL, out = "regression_table_prevalence.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "travel P. falcip. prevalence models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) travel P. falcip. prevalence ~ age, (2) travel P. falcip. prevalence ~ occupation, (3) travel P. falcip. prevalence ~ gender, (4) travel P. falcip. prevalence ~ reason for travel (5) travel P. falcip. prevalence ~ travel means (6) travel P. falcip. prevalence ~ border post (7) travel P. falcip. prevalence ~ home population (8) travel P. falcip. prevalence ~ home distance to nearest urban centre", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)


stargazer (time_age.OUT,time_occupation.OUT,time_gender.OUT,time_reason_travel.OUT, time_means_travel.OUT, 
           time_border_post.OUT, time_pop_HOME.OUT, time_pop_urban_rural_distance.OUT,   type = "html", 
           title = "Coefficient values and significance of variables for univariate logistic models of variables affecting month travelled", style = "default", 
           summary = NULL, out = "regression_table_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Month of travel models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) month of travel ~ age, (2) month of travel ~ occupation, (3) month of travel ~ gender, (4) month of travel ~ reason for travel (5) month of travel ~ travel means (6) month of travel ~ border post (7) month of travel ~ home population (8) month of travel ~ home distance to nearest urban centre", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)

stargazer (time_age.OUT,time_occupation.OUT,time_gender.OUT,time_reason_travel.OUT, time_means_travel.OUT, 
           time_border_post.OUT, time_pop_HOME.OUT, time_pop_urban_rural_distance.OUT,   type = "html", 
           title = "Coefficient values and significance of variables for multivariate linear and lmodels of variables affecting month travelled", style = "default", 
           summary = NULL, out = "regression_table_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Month of travel models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) month of travel ~ age, (2) month of travel ~ occupation, (3) month of travel ~ gender, (4) month of travel ~ reason for travel (5) month of travel ~ travel means (6) month of travel ~ border post (7) month of travel ~ home population (8) month of travel ~ home distance to nearest urban centre", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)

stargazer (distance.OUT,travel_prevalence.OUT, time.OUT,   type = "html", 
           title = "Coefficient values and significance of variables for multivariate linear and lmodels of variables affecting month travelled", style = "default", 
           summary = NULL, out = "regression_table_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Month of travel models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           align = FALSE, 
           coef = NULL, se = NULL, t = NULL, p = NULL,
           t.auto = TRUE, p.auto = TRUE,
           ci = FALSE, ci.custom = NULL,
           ci.level = 0.95, ci.separator = NULL,
           add.lines = NULL, 
           apply.coef = NULL, apply.se = NULL, 
           apply.t = NULL, apply.p = NULL, apply.ci = NULL,
           colnames = NULL,
           column.sep.width = "5pt",
           decimal.mark = NULL, df = TRUE,
           digit.separate = NULL, digit.separator = NULL,
           digits = NULL, digits.extra = NULL, flip = FALSE,
           float = TRUE, float.env="table",
           font.size = NULL, header = TRUE,
           initial.zero = NULL, 
           intercept.bottom = FALSE, intercept.top = TRUE, 
           keep = NULL, keep.stat = NULL,
           label = "", model.names = NULL, 
           model.numbers = NULL, multicolumn = TRUE,
           no.space = NULL,
           notes = "(1) month of travel ~ age, (2) month of travel ~ occupation, (3) month of travel ~ gender, (4) month of travel ~ reason for travel (5) month of travel ~ travel means (6) month of travel ~ border post (7) month of travel ~ home population (8) month of travel ~ home distance to nearest urban centre", notes.align = "l", 
           notes.append = TRUE, notes.label = NULL, 
           object.names = FALSE,
           omit = NULL, omit.labels = NULL, 
           omit.stat = NULL, omit.summary.stat = NULL,
           omit.table.layout = NULL,
           omit.yes.no = c("Yes", "No"), 
           order = NULL, ord.intercepts = FALSE, 
           perl = FALSE, report = NULL, rownames = NULL,
           rq.se = "nid", selection.equation = FALSE, 
           single.row = FALSE,
           star.char = NULL, star.cutoffs = NULL, 
           suppress.errors = FALSE, 
           table.layout = NULL, table.placement = "!htbp",
           zero.component = FALSE, 
           summary.logical = TRUE, summary.stat = NULL,
           nobs = TRUE, mean.sd = TRUE, min.max = TRUE, 
           median = FALSE, iqr = FALSE)

###plotting results

####plotting univariate models
#####time~rural urban distance
library(ggplot2)
high_risk_month_vs_rural_urban_distance_scatter<-ggplot(data=data4,aes(x=data4$HOME_urban_rural_distance..degrees.,y=as.factor(data4$High.risk.month))) +
  geom_point() +
  xlab("Home distance from urban area (degrees)") +
  ylab("") +
  scale_y_discrete(labels = c("Low risk month","High risk month")) +
  labs(title="Distance of home location of those who travelled \nfrom nearest urban centre of occurrance of travel\n in high or low risk months", caption="Source: xx")

fun.1<-function(x) exp((-1.7823*x)-0.5476 )/(1+exp((-1.7823*x)-0.5476 )) #coefficients taken from univariate model for high risk month~rural urban distance
high_risk_month_vs_rural_urban_distance_function<- ggplot(data=data.frame(x=0), aes(x=x)) +
  stat_function(fun=fun.1) +
  xlim(0,1) +
  xlab("Home distance from urban area (degrees)")+
  ylab("Probability of travelling in high risk month") +
  labs(title="Probability a person travels during a high risk month \nbased upon distance of home location from nearest \nurban centre", caption="Source: xx")

####plotting multivariate models
#####log(distance_out)~everything significant

#####falciparum_travel_prevalence ~everything significant
#####as.factor(High.risk.month) ~everything significant



# Outputs -----------------------------------------------------------------


##csvs for arcGIS occupation map
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

##plots for descriptive stats
ggsave("barchart_other_occupations_stacked.png", plot=barchart_other_occupations_stacked,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("pie_other_occupations.png",plot = pie_other_occupations,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("student_seasonal_travel_frequency.png", plot=student_seasonal_travel_frequency, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distribution of distances travelled outside Swaziland for different occupation groups.png", plot=distribution_of_distances_travelled_outside_Swaziland_for_different_occupation_groups, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("month_vs_means_travel_frequency.png",plot=month_vs_means_travel_frequency, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("month_vs_home_urban_rural_distance_scatter.png", plot=month_vs_home_urban_rural_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distances_and_age_class_continuous_distribution.png",plot = distances_and_age_class_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distances_and_gender_continuous_distribution.png", plot=distances_and_gender_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_meanstravel_continuous_distribution.png", plot=distance_and_meanstravel_continuous_distribution, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_borderpost_continuous_distribution.png",plot = distance_and_borderpost_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("pop.HOME_vs_distance_scatter.png",plot=pop.HOME_vs_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("HOME_rural_urban_distance_vs_distance_scatter.png",plot=HOME_rural_urban_distance_vs_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_monthtravel_continuous_distribution",plot=distance_and_monthtravel_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")

ggsave("High_risk_month_vs_Mhlumeni_Goba_travel_frequency.png", plot=High_risk_month_vs_Mhlumeni_Goba_travel_frequency,  path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("High_risk_month_vs_non_Mhlumeni_Goba_travel_frequency.png", plot=High_risk_month_vs_non_Mhlumeni_Goba_travel_frequency,  path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")

##plots for graphs of model outputs 
ggsave("high_risk_month_vs_rural_urban_distance_scatter.png", plot=high_risk_month_vs_rural_urban_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("high_risk_month_vs_rural_urban_distance_function.png", plot=high_risk_month_vs_rural_urban_distance_function,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")






