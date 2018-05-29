



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
install.packages("foreign")
install.packages("nnet")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("haven")
install.packages("gtools")
install.packages("car")
install.packages("lmtest")
install.packages("ggmap")
install.packages("maptools")
install.packages("DT")
install.packages("xlsx")


library(dplyr)
library(knitr)
library(pander)
library(lme4)
library(stargazer)
library(geosphere)
library(tidyverse)
library(foreign)
library(nnet)
library(ggplot2)
library(haven)
library(gtools)
library(lmtest)
library(ggmap)
library(maptools)
library(DT)
library(knitr)
library(xlsx)


# Call data ---------------------------------------------------------------


datain<-read.csv("/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data.csv", header=T)










# Create new variables for addition to datain ----------------------------------------------------

###travel distance variables###
####Distance variable calculated using Lon_EA_Home and lat_EA_Home, lat_to and lon_to (n.b alternatives include: Lat_from_orig and Lon_from_orig)

home_coordinate_data_out<-data.frame(datain$Lon_EA_HOME_distance,datain$lat_EA_HOME_distance)
travel_coordinate_data_out<-data.frame(datain$lon_to,datain$lat_to)
distance_out<-geosphere::distHaversine(home_coordinate_data_out,travel_coordinate_data_out)
which(is.na(lat_travel_out)) # should be 0

datain<-data.frame(datain,distance_out)
datain<-subset(datain,is.na(ID)==FALSE)


# Subset the input data  --------------------------------------------------

data0<-subset(datain,unique.records ==1)

data1 <- subset(datain, Travel_in_out ==2) ##travel in, according to DID travel

data2 <- subset(data1, uniqueTR_In ==1) ##unique records, according to DID travel

data3 <- subset(datain, Travel_in_out ==1) ##travel out, according to DID travel

data4 <- subset(data3, uniqueTR_Out ==1) ##unique records, according to DID travel

data5 <- subset(datain,Travel_in_out ==0)#according to DID NOT travel

data6 <- subset(data5, unique.records ==1)#according to DID NOT travel

data7 <- subset(datain, unique.records ==1) #all dataset unique records

#n.b type 1 corresponds to Index case identification carried out primarily through passive case investigation (2010-2014) ; type 2 corresponds to Reactive case detection (RACD), in which all people residing within 1km (January 2010 to June 2013) or 500m (July 2013 to June 2014) radius of index case are surveyed (and subsequently referred for treatment if tested positive) (2012-2014 only)


# Create new datasets -----------------------------------------------------

###age variable select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving occupation_code 
data4$selected_age<-  factor(data4$age_class, levels=c('1','2','3','4','5','6','7')) 
data4$selected_age_anynightsaway = data4$selected_age
table(data4$selected_age,useNA = "always")
data4$selected_age_anynightsaway[-which(data4$nightsaway!=""&data4$nightsaway!="-Inf"&data4$nightsaway!="Inf"&data4$nightsaway!="0")] <- NA
#data4_selected_age_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$age_class, levels=c('1','2','3','4','5','6','7')) 
###occupation variable select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving occupation_code 
data4_selected_occupations<-  factor(data4$Occupation_code, levels=c('1','2','3','4','5','6','8','9')) 
data4_selected_occupations_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$Occupation_code, levels=c('1','2','3','4','5','6','8','9')) 
###country variable select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_selected_countries<-  factor(data4$Country, levels=c("Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Unknown","Zambia","Zimbabwe")) 
data4_selected_countries_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$Country, levels=c("Equitorial Guinea","Ethiopia","Ghana","Malawi","Mozambique","Nigeria","Rwanda","South Africa","Tanzania","Uganda","Unknown","Zambia","Zimbabwe")) 


###gender variable select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving Country
data4_selected_genders<-  factor(data4$Gender, levels=c("F","M"))
data4_selected_genders_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$Gender, levels=c("F","M"))

###reason travel variable select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving reason travel
data4_selected_ReasonTravel<-  factor(data4$ReasonTravel, levels=c("Business" , "Holiday",  "Other"  ,  "Visiting"))
data4_selected_ReasonTravel_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$ReasonTravel, levels=c("Business" , "Holiday",  "Other"  ,  "Visiting"))

###means travel variable###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving means travel
data4_selected_MeansTravel<-  factor(data4$MeansTravel, levels=c("Personal Car" ,"Airplane","Bicycle","Kombi (van)" , "Large bus"  , "Ride share"  , "Truck"   ,     "Walked" ))
data4_selected_MeansTravel_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$MeansTravel, levels=c("Personal Car" ,"Airplane","Bicycle","Kombi (van)" , "Large bus"  , "Ride share"  , "Truck"   ,     "Walked" ))

###Border Post variabl select###
#####with this step you can select the order of the variable, drop values and select which will be the value where you compared the others against so it goes first in the list.
#####these occupation variables should be used for all analyses involving border post
data4_selected_BorderPost<-  factor(data4$BorderPost, levels=c("Lavumisa/Golela"   ,   "Lomahasha/Namaacha"  , "Mahamba"          ,    "Mananga"       ,       "Matsamo/Jeppe's Reef" ,"Matsapha Airport"  ,  "Mhlumeni/Goba"      ,  "Ngwenya/Oshoek"   ,    "Other"     ,           "Salitje/Onverwacht"  , "Sandlane/Nerston"  ,   "Sicunusa/Houtkop"  ))
data4_selected_BorderPost_nightsawaydata<-  factor(subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0")$BorderPost, levels=c("Lavumisa/Golela"   ,   "Lomahasha/Namaacha"  , "Mahamba"          ,    "Mananga"       ,       "Matsamo/Jeppe's Reef" ,"Matsapha Airport"  ,  "Mhlumeni/Goba"      ,  "Ngwenya/Oshoek"   ,    "Other"     ,           "Salitje/Onverwacht"  , "Sandlane/Nerston"  ,   "Sicunusa/Houtkop"  ))




###datasets with falciparum_prevalence variable###
####use for all falciparum_prevalence analysis

length(data4$ID)#number of falciparum non-NA values in data4
length((subset(data4,is.na(falciparum_travel_prevalence)==TRUE))$ID)#total number of NAs in data4 falciparum travel prevalence
length(subset(data4,fal_12==-9999)$ID)#number of data4 missing falciparum values due to missing MAP falciparum values 
length(subset(data4,is.na(Year.travel))$ID)#number of data4 missing falciparum values due to missing 'Year.travel' in data
###some falcip values missing from data4 due to a)missing year travel value and b)mising malaria atlas project data.


###manual workers dataset###
data4_manual_workers<- subset(data4,Occupation_code==5)

###students dataset
data4_students<-subset(data4,Occupation_code==8)

###BorderPost datasets
data4_Mhlumeni_Goba<-subset(data4,BorderPost=="Mhlumeni/Goba")
data4_not_Mhlumeni_Goba<-subset(data4,BorderPost!="Mhlumeni/Goba")
table(data4$Occupation_code)
####data with distance variable by occupation code
data4_occupation_code1=subset(data4,Occupation_code==1)
length(data4_occupation_code1$ID)
data4_occupation_code2=subset(data4,Occupation_code==2)
length(data4_occupation_code2$ID)
data4_occupation_code3=subset(data4,Occupation_code==3)
length(data4_occupation_code3$ID)
data4_occupation_code4=subset(data4,Occupation_code==4)
length(data4_occupation_code4$ID)
data4_occupation_code5=subset(data4,Occupation_code==5)
length(data4_occupation_code5$ID)
data4_occupation_code6=subset(data4,Occupation_code==6)
length(data4_occupation_code6$ID)
data4_occupation_code7=subset(data4,Occupation_code==7)
length(data4_occupation_code7$ID)
data4_occupation_code8=subset(data4,Occupation_code==8)
length(data4_occupation_code8$ID)
data4_occupation_code9=subset(data4,Occupation_code==9)
length(data4_occupation_code9$ID)
data4_occupation_code10=subset(data4,Occupation_code==10)
length(data4_occupation_code10$ID)
data4_occupation_codeNA=subset(data4,is.na(Occupation_code)==TRUE)
length(data4_occupation_codeNA$ID)#these should total length(data4$ID)

####give occupation code subset datasets unique distance_out names for use in ggplot of distance distributions
colnames(data4_occupation_code1)[216]<-"distance_out_occupation1"
colnames(data4_occupation_code2)[216]<-"distance_out_occupation2"
colnames(data4_occupation_code3)[216]<-"distance_out_occupation3"
colnames(data4_occupation_code4)[216]<-"distance_out_occupation4"
colnames(data4_occupation_code5)[216]<-"distance_out_occupation5"
colnames(data4_occupation_code6)[216]<-"distance_out_occupation6"
colnames(data4_occupation_code7)[216]<-"distance_out_occupation7"
colnames(data4_occupation_code8)[216]<-"distance_out_occupation8"
colnames(data4_occupation_code9)[216]<-"distance_out_occupation9"
colnames(data4_occupation_code10)[216]<-"distance_out_occupation10"














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
data4_malariapositive<-subset(data4, Malaria_0_1==1)
data4_malarianegative<-subset(data4, Malaria_0_1==0)
data4_yeartravel_2010<-subset(datain, Year.travel==2010)
data4_TYPE2or3<- subset(data4,TYPE==2|TYPE==3)
data4_TYPE_2_malariapositive<-subset(data4_TYPE_2,Malaria_0_1==1)
data4_TYPE_3_malariapositive<-subset(data4_TYPE_3,Malaria_0_1==1)
data4_malariapositive<-subset(data4,Malaria_0_1==1)
data4_malarianegative<-subset(data4,Malaria_0_1==0)
data4_malariapositive<-subset(data4,Malaria_0_1==1)
data4_malarianegative<-subset(data4,Malaria_0_1==0)



mean(data4_malariapositive$distance_out)/1000
mean(data4_malarianegative$distance_out)/1000
mean(data4_malariapositive$falciparum_travel_prevalence)
mean(data4_malarianegative$falciparum_travel_prevalence)
length(data4_malariapositive_malariaseason$ID)/length(data4_malariapositive$ID)
length(data4_malarianegative_malariaseason$ID)/length(data4_malarianegative$ID)
mean((data4_malariapositive$nightsaway),na.rm = TRUE)
mean((data4_malarianegative$nightsaway),na.rm = TRUE)

data4negative<-subset(data4,Malaria_0_1==0)
data4positive<-subset(data4,Malaria_0_1==1)
median(subset(data4positive,is.na(nightsaway)==FALSE)$nightsaway)
median(subset(data4negative,is.na(nightsaway)==FALSE)$nightsaway)
mean(subset(data4positive,is.na(nightsaway)==FALSE)$nightsaway)
mean(subset(data4negative,is.na(nightsaway)==FALSE)$nightsaway)
length(subset(data4negative, High.risk.month==1)$ID)/length(data4negative$ID)
length(subset(data4positive, High.risk.month==1)$ID)/length(data4positive$ID)


MonthMeanFalcipTable<-aggregate(data4[,c("falciparum_travel_prevalence","distance_out", "Age","nightsaway")], by=list("Month.travel"=data4$Month.travel), FUN= "mean", na.rm=TRUE)
MonthSDFalcipTable<-aggregate(data4[,c("falciparum_travel_prevalence","distance_out", "Age","nightsaway")], by=list("Month.travel"=data4$Month.travel), FUN= "sd", na.rm=TRUE)
MonthMeanSdTable<- ddply(data4, c("Month.travel"), summarise,
                         n_month = length(Month.travel),
                         distance_sd   = sd(distance_out, na.rm=TRUE),
                         distance_mean = mean(distance_out, na.rm=TRUE),
                         falcip_sd  = sd(falciparum_travel_prevalence, na.rm=TRUE),
                         falcip_mean = mean(falciparum_travel_prevalence, na.rm=TRUE)
)
MonthReasonHolidayTable<- ddply(subset(data4, ReasonTravel=="Holiday"), c("Month.travel"), summarise,
                         n = length(ReasonTravel))
MonthReasonVisitingTable<- ddply(subset(data4, ReasonTravel=="Visiting"), c("Month.travel"), summarise,
                                n = length(ReasonTravel))
MonthReasonOtherTable<- ddply(subset(data4, ReasonTravel=="Other"), c("Month.travel"), summarise,
                                 n = length(ReasonTravel))
MonthReasonBusinessTable<- ddply(subset(data4, ReasonTravel=="Business"), c("Month.travel"), summarise,
                              n = length(ReasonTravel))
HighRiskMonthMeanSdTable<- ddply(data4, c("High.risk.month"), summarise,
                         n_month = length(Month.travel),
                         distance_sd   = sd(distance_out, na.rm=TRUE),
                         distance_mean = mean(distance_out, na.rm=TRUE),
                         falcip_sd  = sd(falciparum_travel_prevalence, na.rm=TRUE),
                         falcip_mean = mean(falciparum_travel_prevalence, na.rm=TRUE),
                         age_mean = mean(Age, na.rm=TRUE),
                         age_sd = sd(Age, na.rm=TRUE),
                         distance_max = max(distance_out, na.rm = TRUE),
                         distance_min = min(distance_out, na.rm = TRUE)
)
AgeGroupNTable<- ddply(data4, c("age_class"), summarise,
                                 n_age_class = length(age_class))
OccupationCodeNTable<- ddply(data4, c("Occupation_code"), summarise,
                       n_age_class = length(Occupation_code))
CountryNTable<- ddply((subset(data4, High.risk.month=="0")), c("Country"), summarise,
                             n_country = length(Country))


write.csv(MonthMeanFalcipTable, file = "MonthMeanFalcipTable.csv")
library(plyr)
CountryMeanSDFalcipTable <- ddply(data4, c("Country"), summarise,
                                     n_country = length(Country),
                                     distance_sd   = sd(distance_out, na.rm=TRUE),
                                     distance_mean = mean(distance_out, na.rm=TRUE),
                                     falcip_sd  = sd(falciparum_travel_prevalence, na.rm=TRUE),
                                     falcip_mean = mean(falciparum_travel_prevalence, na.rm=TRUE)
                                    )
mean_falcip_by_country_bar<-ggplot(data=CountryMeanSDFalcipTable, aes(x=Country,y=falcip_mean)) + geom_col()
       
month_vs_mean_prevalence_bar<-qplot(x=as.factor(MonthMeanFalcipTable$Month.travel),y=as.factor(MonthMeanFalcipTable$falciparum_travel_prevalence), geom="col")
month_vs_mean_distance_bar<-qplot(x=as.factor(MonthMeanFalcipTable$Month.travel),y=as.factor(MonthMeanFalcipTable$distance_out), geom="col")
month_vs_distance_scatter<-qplot(x=month(as.Date(data4$FirstEntered, format="%d/%m/%Y")),y=data4$distance_out)
month_vs_prevalence_scatter<-qplot(x=month(as.Date(data4$FirstEntered, format="%d/%m/%Y")),y=data4$falciparum_travel_prevalence)
country_vs_prevalence_scatter<-qplot(x=data4$Country,y=data4$falciparum_travel_prevalence)
country_vs_distance_scatter<-qplot(x=data4$Country,y=data4$distance_out)
country_vs_sd_prevalence_scatter<-qplot(x=CountryMeanSDFalcipTable$Country,y=CountryMeanSDFalcipTable$falcip_sd)
country_vs_sd_distance_scatter<-qplot(x=CountryMeanSDFalcipTable$Country,y=CountryMeanSDFalcipTable$distance_sd)
country_vs_mean_prevalence_scatter<-qplot(x=CountryMeanSDFalcipTable$Country,y=CountryMeanSDFalcipTable$falcip_mean)
country_vs_mean_distance_scatter<-qplot(x=CountryMeanSDFalcipTable$Country,y=CountryMeanSDFalcipTable$distance_mean)


               



###overview of datain
library(lubridate)
library(ggplot2)

datain$newdate = as.Date(datain$FirstEntered.1,format = "%d/%m/%Y")
datain$newmonth = as.factor(month(datain$newdate))
datain$newmonth2 = relevel(datain$newmonth,ref="6")

summary(glm(data=datain,Malaria_0_1 ~ newmonth2))
# Significantly more malaria during Jan, Feb, May, compared to June

datain$TravelOS2 = as.numeric(datain$TravelOS == "Yes") # shortcut to turn travelos into a binary variable, this will be 1 if true, 0 if false
summary(glm(data=datain, TravelOS2 ~ newmonth2,family="binomial"))
#Significantly more travel in Jan and Feb, significantly less in Sept, Oct, Dec

histogram_cases_per_travel_month_datain<-ggplot() + geom_histogram(data=datain,aes(x=newmonth,fill=TravelOS),stat="count",bins=12)+ 
  scale_fill_brewer(palette="Set1",name="Travel outside Swaziland")+
  ylab("Number of cases")+
  xlab("Month data first entered")+
  labs(title="Histogram to show number of +ve and -ve \ncases recorded in each month, 2010-2014") + 
  theme_classic() +
  theme(legend.position = "bottom") 
histogram_cases_per_travel_month_datain

histogram_cases_per_firstentered_month_datain<-ggplot() + geom_histogram(data=datain,aes(x=newmonth,fill=as.factor(Malaria_0_1)),stat="count",bins=12)+
  scale_fill_brewer(palette="Set1",name="Malaria",breaks=c(0,1),labels=c("Negative","Positive"))+
  ylab("Number of cases")+
  xlab("Month data first entered")+
  labs(title="Histogram to show number of +ve and -ve \ncases recorded in each month, 2010-2014") +
  theme_classic() +
  theme(legend.position = "bottom") 
histogram_cases_per_firstentered_month_datain

######xx REDO THESE PLOTS TO LOOK SAME AS NICK'S 






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
data4_occupation_code1=subset(data4,Occupation_code==1)
length(data4_occupation_code1$ID)
data4_occupation_code2=subset(data4,Occupation_code==2)
length(data4_occupation_code2$ID)
data4_occupation_code3=subset(data4,Occupation_code==3)
length(data4_occupation_code3$ID)
data4_occupation_code4=subset(data4,Occupation_code==4)
length(data4_occupation_code4$ID)
data4_occupation_code5=subset(data4,Occupation_code==5)
length(data4_occupation_code5$ID)
data4_occupation_code6=subset(data4,Occupation_code==6)
length(data4_occupation_code6$ID)
data4_occupation_code7=subset(data4,Occupation_code==7)
length(data4_occupation_code7$ID)
data4_occupation_code8=subset(data4,Occupation_code==8)
length(data4_occupation_code8$ID)
data4_occupation_code9=subset(data4,Occupation_code==9)
length(data4_occupation_code9$ID)
data4_occupation_code10=subset(data4,Occupation_code==10)
length(data4_occupation_code10$ID)
data4_occupation_codeNA=subset(data4,is.na(Occupation_code)==TRUE)
length(data4_occupation_codeNA$ID)#these should total length(data4$ID)

####
data4selected_occ = subset(data4,is.element(Occupation_code,c(1:8)))
distribution_of_distances_travelled_outside_Swaziland_for_different_occupation_groups2<- ggplot() +
  geom_density(data=data4selected_occ, aes(x=distance_out, colour=as.factor(Occupation_code))) +
   scale_colour_manual("", 
                      breaks = c(1,2,3,4,5,6,7,8),
                      values = c("grey", "grey", "grey","blue","green","red","grey","grey"),
                      labels = c("Unemployed","Farming/Agriculture","Manufacturing/Factory","Office/Clerical Work","Other","Other Manual Labour","Small-market sales or trade","Student")) +
  labs(x="Distance travelled (m)",y="Probability density",fill="Occupation",title = "Continuous distibution of distances travelled outside Swaziland \nfor different occupation groups") +
  theme_classic() +
  theme(legend.position = "bottom") 

distribution_of_distances_travelled_outside_Swaziland_for_different_occupation_groups<- ggplot() +
  geom_density(data=data4_occupation_code1, aes(x=data4_occupation_code1$distance_out, colour="data4_occupation_code1$distance_out")) +
  geom_density(data=data4_occupation_code2, aes(x=data4_occupation_code2$distance_out, colour="data4_occupation_code2$distance_out")) +
  geom_density(data=data4_occupation_code3, aes(x=data4_occupation_code3$distance_out, colour="data4_occupation_code3$distance_out")) +
  geom_density(data=data4_occupation_code4, aes(x=data4_occupation_code4$distance_out, colour="data4_occupation_code4$distance_out")) +
  geom_density(data=data4_occupation_code5, aes(x=data4_occupation_code5$distance_out, colour="data4_occupation_code5$distance_out")) +
  geom_density(data=data4_occupation_code6, aes(x=data4_occupation_code6$distance_out, colour="data4_occupation_code6$distance_out")) +
  geom_density(data=data4_occupation_code7, aes(x=data4_occupation_code7$distance_out, colour="data4_occupation_code7$distance_out")) +
  geom_density(data=data4_occupation_code8, aes(x=data4_occupation_code8$distance_out, colour="data4_occupation_code8$distance_out")) +
  scale_colour_manual("", 
                      breaks = c("data4_occupation_code1$distance_out", "data4_occupation_code2$distance_out","data4_occupation_code3$distance_out","data4_occupation_code4$distance_out","data4_occupation_code5$distance_out","data4_occupation_code6$distance_out","data4_occupation_code7$distance_out","data4_occupation_code8$distance_out"),
                      values = c("grey", "grey", "grey","blue","green","red","grey","grey"),
                      labels = c("Unemployed","Farming/Agriculture","Manufacturing/Factory","Office/Clerical Work","Other","Other Manual Labour","Small-market sales or trade","Student")) +
  labs(x="Distance travelled (m)",y="Probability density",fill="Occupation",title = "Continuous distibution of distances travelled outside Swaziland \nfor different occupation groups") +
  theme_classic() +
  theme(legend.position = "bottom") 


###distance and age_class
data4_age_class5<-subset(data4,age_class==5)
data4_age_class_not_5<-subset(data4,age_class!=5)
library(ggplot2)
distances_and_age_class_continuous_distribution<-  ggplot()+
  geom_density(data = data4_age_class5,aes(x=data4_age_class5$distance_out,color=("25<=age<45"))) + 
  geom_density(data=data4_age_class_not_5,aes(x=data4_age_class_not_5$distance_out,color=("all other ages"))) +
  scale_colour_manual( name="",
                      values = c("red", "grey")) +
  labs(x="Distance travelled (m)",y="Probability density",title = "Continuous distibution of distances travelled outside Swaziland \nfor different age groups") +
  theme_classic() +
  theme(legend.position = "bottom") 


  
###distance and gender
data4_male<-subset(data4,Gender=="M")
data4_female<-subset(data4, Gender=="F")
data4_gender_NA<-subset(data4,Gender="U")
library(ggplot2)
distances_and_gender_continuous_distribution<-   ggplot() +
  geom_density(data=data4_male,aes(x=data4_male$distance_out,color="Male")) +
  geom_density(data=data4_female, aes(x=data4_female$distance_out, color="Female")) +
  scale_color_manual(name="",values = c("red","blue")) + 
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor males and females") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and means travel
data4_Airplane<-subset(data4,MeansTravel=="Airplane")
data4_Bicycle<-subset(data4,MeansTravel=="Bicycle")
data4_Kombi<-subset(data4,MeansTravel=="Kombi (van)")
data4_Largebus<-subset(data4,MeansTravel=="Large bus")
data4_PersonalCar<-subset(data4,MeansTravel=="Personal Car")
data4_Rideshare<-subset(data4,MeansTravel=="Ride share")
data4_Truck<-subset(data4,MeansTravel=="Truck")
data4_Walked<-subset(data4,MeansTravel=="Walked")
data4_meanstravel_NA<-subset(data4,MeansTravel=="")
library(ggplot2)
distance_and_meanstravel_continuous_distribution<- ggplot() +
  geom_density(data=data4_Airplane,aes(x=data4_Airplane$distance_out,color="Airplane")) +
  geom_density(data=data4_Kombi,aes(x=data4_Kombi$distance_out,color="Kombi (van)")) +
  geom_density(data=data4_Largebus,aes(x=data4_Largebus$distance_out,color="Large Bus")) +
  geom_density(data=data4_PersonalCar,aes(x=data4_PersonalCar$distance_out,color="Personal Car")) +
  geom_density(data=data4_Rideshare,aes(x=data4_Rideshare$distance_out,color="Ride Share")) +
  geom_density(data=data4_Walked,aes(x=data4_Walked$distance_out,color="Walked")) +
  geom_density(data=data4_meanstravel_NA,aes(x=data4_meanstravel_NA$distance_out,color="NA")) +
  
  scale_color_manual(name="", values=c("grey","blue","yellow","brown","violet","red","orange","pink","green")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different modes of transport") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and border posts
data4_LavumisaGolela<-subset(data4,BorderPost=="Lavumisa/Golela")
data4_LomahashaNamaacha<-subset(data4,BorderPost=="Lomahasha/Namaacha")
data4_Mahamba<-subset(data4,BorderPost=="Mahamba")
data4_Mananga<-subset(data4,BorderPost=="Mananga")
data4_MatsamoJeppesReef<-subset(data4,BorderPost=="Matsamo/Jeppe\'s Reef")
data4_MatsaphaAirport<-subset(data4,BorderPost=="Matsapha Airport")
data4_MhlumeniGoba<-subset(data4,BorderPost=="Mhlumeni/Goba")
data4_NgwenyaOshoek<-subset(data4,BorderPost=="Ngwenya/Oshoek")
data4_SalitjeOnverwacht<-subset(data4,BorderPost=="Salitje/Onverwacht")
data4_SandlaneNerston<-subset(data4,BorderPost=="Sandlane/Nerston")
data4_SicunusaHoutkop<-subset(data4,BorderPost=="Sicunusa/Houtkop")

library(ggplot2)
distance_and_borderpost_continuous_distribution<- ggplot() +
  geom_density(data=data4_LavumisaGolela,aes(x=data4_LavumisaGolela$distance_out,color="Lavumisa/Golela")) +
  geom_density(data=data4_LomahashaNamaacha,aes(x=data4_LomahashaNamaacha$distance_out,color="Lomahasha/Namaacha")) +
  geom_density(data=data4_Mahamba,aes(x=data4_Mahamba$distance_out,color="Mahamba")) +
  geom_density(data=data4_Mananga,aes(x=data4_Mananga$distance_out,color="Manangar")) +
  geom_density(data=data4_MatsamoJeppesReef,aes(x=data4_MatsamoJeppesReef$distance_out,color="Matsamo/Jeppe\'s Reef")) +
  geom_density(data=data4_MatsaphaAirport,aes(x=data4_MatsaphaAirport$distance_out,color="Matsapha Airport")) +
  geom_density(data=data4_MhlumeniGoba,aes(x=data4_MhlumeniGoba$distance_out,color="Mhlumeni/Goba")) +
  geom_density(data=data4_NgwenyaOshoek,aes(x=data4_NgwenyaOshoek$distance_out,color="Ngwenya/Oshoek")) +
  geom_density(data=data4_SalitjeOnverwacht,aes(x=data4_SalitjeOnverwacht$distance_out,color="Salitje/Onverwacht")) +
  geom_density(data=data4_SandlaneNerston,aes(x=data4_SandlaneNerston$distance_out,color="Sandlane/Nerston")) +
  geom_density(data=data4_SicunusaHoutkop,aes(x=data4_SicunusaHoutkop$distance_out,color="Sicunusa/Houtkop")) +
  
  scale_color_manual(name="", values=c("grey","blue","yellow","brown","black","red","orange","grey","green","beige","darkred")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different border crossings") +
  theme_classic() +
  theme(legend.position = "bottom")



###distance and pop home
library(ggplot2)
  pop.HOME_vs_distance_scatter<- ggplot(data=data4,aes(x=data4$pop.HOME,y=data4$distance_out)) +
  geom_point() +
  labs(x="Population of home locality", y="Distance travelled (m)", title="Relationship between home population and distance travelled") +
    theme_classic() +
    theme(legend.position = "bottom")
  
###distance and Home urban_rural distance
library(ggplot2)
HOME_rural_urban_distance_vs_distance_scatter<- ggplot(data=data4,aes(x=data4$HOME_urban_rural_distance..degrees.,y=data4$distance_out)) +
  geom_point() +
  labs(x="Distance to nearest urban centre (degrees)", y="Distance travelled (m)", title="Relationship between distance of home from nearest urban centre \nand distance travelled") +
  theme_classic() +
  theme(legend.position = "bottom")

###distance and month travel
data4_jan<-subset(data4,Month.travel=="1")
data4_feb<-subset(data4,Month.travel=="2")
data4_mar<-subset(data4,Month.travel=="3")
data4_apr<-subset(data4,Month.travel=="4")
data4_may<-subset(data4,Month.travel=="5")
data4_jun<-subset(data4,Month.travel=="6")
data4_jul<-subset(data4,Month.travel=="7")
data4_aug<-subset(data4,Month.travel=="8")
data4_sep<-subset(data4,Month.travel=="9")
data4_oct<-subset(data4,Month.travel=="10")
data4_nov<-subset(data4,Month.travel=="11")
data4_dec<-subset(data4,Month.travel=="12")
data4_high_risk_month

library(ggplot2)
distance_and_monthtravel_continuous_distribution<- ggplot() +
  geom_density(data=data4_jan,aes(x=data4_jan$distance_out,color="Jan")) +
  geom_density(data=data4_feb,aes(x=data4_feb$distance_out,color="Feb")) +
  geom_density(data=data4_mar,aes(x=data4_mar$distance_out,color="Mar")) +
  geom_density(data=data4_apr,aes(x=data4_apr$distance_out,color="Apr")) +
  geom_density(data=data4_may,aes(x=data4_may$distance_out,color="May")) +
  geom_density(data=data4_jun,aes(x=data4_jun$distance_out,color="Jun")) +
  geom_density(data=data4_jul,aes(x=data4_jul$distance_out,color="Jul")) +
  geom_density(data=data4_aug,aes(x=data4_aug$distance_out,color="Aug")) +
  geom_density(data=data4_sep,aes(x=data4_sep$distance_out,color="Sep")) +
  geom_density(data=data4_oct,aes(x=data4_oct$distance_out,color="Oct")) +
  geom_density(data=data4_nov,aes(x=data4_nov$distance_out,color="Nov")) +
  geom_density(data=data4_dec,aes(x=data4_dec$distance_out,color="Dec")) +
  scale_color_manual(name="", breaks=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),values=c("grey","blue","yellow","brown","black","red","orange","grey","green","beige","darkred","lightblue")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled outside Swaziland \nfor different months, 2012-2014") +
  theme_classic() +
  theme(legend.position = "bottom")

library(ggplot2)
distance_and_time_binary_continuous_distribution<- ggplot() +
  geom_density(data=subset(data4,Month.travel==11|Month.travel==12|Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4|Month.travel==5),aes(x=distance_out,color="High.risk.month")) +
  geom_density(data=subset(data4,Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9|Month.travel==10),aes(x=distance_out,color="Low.risk.month")) +
  scale_color_manual(name="", breaks=c("High.risk.month","Low.risk.month"),values=c("red","blue")) +
  labs(x="Distance travelled (m)", y="Probability density", title="Continuous distribution of distances travelled \noutside Swaziland for cases travelling in high and low \nmalaria transmission risk months, 2012-2014") +
  theme_classic() +
  theme(legend.position = "bottom")
distance_and_time_binary_continuous_distribution

library(ggplot2)
distance_and_time_binary_continuous_distribution_histogram<- ggplot() +
  geom_histogram(data=subset(data4,Month.travel==11|Month.travel==12|Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4|Month.travel==5),aes(x=distance_out, fill="High.risk.month")) +
  geom_histogram(data=subset(data4,Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9|Month.travel==10),aes(x=distance_out, fill="Low.risk.month")) +
  scale_color_manual(name="", breaks=c("High.risk.month","Low.risk.month"),values=c("red","blue")) +
  labs(x="Distance travelled (m)", y="Frequency", title="Histogram of distances travelled \noutside Swaziland for cases travelling in high and low \nmalaria transmission risk months, 2012-2014", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") 
distance_and_time_binary_continuous_distribution_histogram




###prevalence frequencies
library(ggplot2)
prevalence_and_time_binary_continuous_distribution<- ggplot() +
  geom_density(data=subset(data4,Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4),aes(x=falciparum_travel_prevalence,color="High.risk.month")) +
  geom_density(data=subset(data4,Month.travel==5|Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9|Month.travel==10|Month.travel==11|Month.travel==12),aes(x=falciparum_travel_prevalence,color="Low.risk.month")) +
  scale_color_manual(name="", breaks=c("High.risk.month","Low.risk.month"),values=c("red","blue")) +
  labs(x="P.falcip. prevalence at destination (proportion)", y="Probability density", title="Continuous distribution of P. falcip. prevalence at \ntravel destination outside Swaziland for cases travelling in \nhigh and low malaria transmission risk months, 2012-2014") +
  theme_classic() +
  theme(legend.position = "bottom")
prevalence_and_time_binary_continuous_distribution

library(ggplot2)
prevalence_and_time_binary_continuous_distribution_histogram<- ggplot() +
  geom_histogram(data=subset(data4,Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4),aes(x=falciparum_travel_prevalence, fill="High.risk.month")) +
  geom_histogram(data=subset(data4,Month.travel==5|Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9|Month.travel==10|Month.travel==11|Month.travel==12),aes(x=falciparum_travel_prevalence, fill="Low.risk.month")) +
  scale_color_manual(name="", breaks=c("High.risk.month","Low.risk.month"),values=c("red","blue")) +
  labs(x="P.falcip. prevalence at destination (proportion)", y="Frequency", title="Histogram of P. falcip. prevalence at \ntravel destination outside Swaziland for cases travelling in high and low \nmalaria transmission risk months, 2012-2014", fill = "") +
  theme_classic() +
  theme(legend.position = "bottom") 
prevalence_and_time_binary_continuous_distribution_histogram








##time frequencies

###number of cases of malaria in each month 
library(ggplot2)
malaria_cases_each_travel_month<- ggplot(data=subset(data4,Month.travel!=""), aes(as.factor(subset(data4,Month.travel!="")$Month.travel), fill = as.factor(subset(data4,Month.travel!="")$Malaria_0_1))) +
  geom_bar(na.rm = TRUE) + 
  scale_fill_brewer(palette="Set1",name="Malaria", breaks=c(0,1),labels=c("Negative","Positive"))+
  labs(title = "Histogram showing frequency of travel and proportion \nof malaria positive cases for different months \ntravelled, 2012-2014") +
  xlab("Month of travel") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
malaria_cases_each_travel_month

library(ggplot2)
cases_each_travel_month<- ggplot(data=subset(data4,Month.travel!=""), aes(as.factor(subset(data4,Month.travel!="")$Month.travel))) +
  geom_bar(na.rm = TRUE) + 
  scale_fill_brewer(palette="Set1",name="Malaria", breaks=c(0,1),labels=c("Negative","Positive"))+
  labs(title = "Histogram showing frequency of travel /nfor different months, 2012-2014") +
  xlab("Month of travel") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
cases_each_travel_month 

library(ggplot2)
reasontravel_cases_each_travel_month<- ggplot(data=subset(data4,Month.travel!=""), aes(as.factor(subset(data4,Month.travel!="")$Month.travel), fill = as.factor(subset(data4,Month.travel!="")$ReasonTravel))) +
  geom_bar(na.rm = TRUE) + 
  scale_fill_brewer(palette="Set1",name="Reason for travel", breaks=c("Business","Holiday", "Other", "Visiting"),labels=c("Business","Holiday", "Other", "Visiting"))+
  labs(title = "Histogram showing frequency of travel by  \nreason for travel for different months \ntravelled, 2012-2014") +
  xlab("Month of travel") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
reasontravel_cases_each_travel_month


library(ggplot2)
malaria_cases_each_travel_firstentered<- ggplot(data=subset(data4,FirstEntered!=""), aes(as.factor(month(as.Date(subset(data4,FirstEntered!="")$FirstEntered,format = "%d/%m/%Y"))), fill = as.factor(subset(data4,FirstEntered!="")$Malaria_0_1))) +
  geom_bar() + 
  scale_fill_brewer(palette="Set1",name="Malaria", breaks=c(0,1),labels=c("Negative","Positive"))+
  labs(title = "Histogram showing frequency month first entered for \ntravel cases and proportion of malaria positive cases \nrecorded in different months, 2012-2014") +
  xlab("Month data first entered") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
malaria_cases_each_travel_firstentered

###number of malaria cases over time 
library(ggplot2)
malaria_cases_2010_2014<- ggplot(data=datain, aes(as.Date(datain$FirstEntered,format="%d/%m/%Y"),fill= as.factor(Malaria_0_1)) ) +
  geom_histogram(bins = "42") + 
  scale_fill_manual("legend",values = c("orange","blue"), labels=c("Malaria negative", "Malaria positive")) +
  labs(title = "Histogram showing frequency of all cases \nby month over time (positive and negative,\n travelled and did not travel ), 2012-2014") +
  xlab("Date") +
  ylab("Frequency (total no. of cases)") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_date( breaks= date_breaks("6 months"))
malaria_cases_2010_2014





###number of students who travel in each month
library(ggplot2)
other_manual_labourer_seasonal_travel_frequency<- ggplot(data=subset(data4,Occupation_code==6),aes(as.factor(subset(data4,Occupation_code==6)$Month.travel), fill = as.factor(subset(data4,Occupation_code==6)$High.risk.month))) +
  geom_bar() + 
  scale_fill_manual("legend",values = c("red", "yellow","orange","blue"), labels=c( "Low risk month","High risk month")) +
  labs(title = "Histogram showing frequency of \nother manual labour travel in different months, 2012-2014") +
  xlab("Month of travel") +
  ylab("Frequency") +
  theme (text = element_text(face="plain",size = 8)) +
  theme (plot.title = element_text(size = rel(1.5),face="bold")) +
  theme (axis.title = element_text(size = rel(1.2),face="bold")) +
  theme (legend.position = "bottom") +
  scale_x_discrete(name= waiver(),position = "bottom", labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
other_manual_labourer_seasonal_travel_frequency


###number of people using different transport types in each month 
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
  
###Number of people travelling through different border posts in high risk and low risk months
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

###Urban rural distance for people travelling in different months
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


##Breakdown of age class by 'travelled in high or low risk month' 
###Travelled in high risk month
age_class_high_risk_months_table<-table(subset(data4, Month.travel==10|Month.travel==11|Month.travel==12|Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4)$age_class) 
age_class_low_risk_months_table<-table(subset(data4, Month.travel==5|Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9)$age_class)

occupation_high_risk_months_table<-table(subset(data4, Month.travel==10|Month.travel==11|Month.travel==12|Month.travel==1|Month.travel==2|Month.travel==3|Month.travel==4)$Occupation_code) 
occupation_low_risk_months_table<-table(subset(data4, Month.travel==5|Month.travel==6|Month.travel==7|Month.travel==8|Month.travel==9)$Occupation_code)













##Duration frequencies xx tbc 
###age of people travelling for different durations xx
###reason travel for people travelling for different durations xx 
###border crossing for people travelling for different durations xx 


###duration and age_class
data4_age_class5<-subset(data4,age_class==5)
data4_age_class6<-subset(data4,age_class==6)
data4_age_class_not_5_or_6<-subset(data4,age_class!=5&age_class!=6)
library(ggplot2)
durations_and_age_class_continuous_distribution<-  ggplot()+
  geom_density(data = data4_age_class5,aes(x=log(data4_age_class5$nightsaway),color=("25<=age<45"))) + 
  geom_density(data = data4_age_class6,aes(x=log(data4_age_class6$nightsaway),color=("45<=age<65"))) +
  geom_density(data=data4_age_class_not_5_or_6,aes(x=log(data4_age_class_not_5_or_6$nightsaway),color=("all other ages"))) +
  scale_colour_manual( name="",
                       values = c("red", "blue", "grey")) +
  labs(x="Travel duration (nights away)",y="Probability density",title = "Continuous distibution for duration of travel outside Swaziland \nfor different age groups") +
  theme_classic() +
  theme(legend.position = "bottom") 

###duration and reason_travel
data4_business<-subset(data4,ReasonTravel=="Business")
data4_holiday<-subset(data4,ReasonTravel=="Holiday")
data4_other<-subset(data4,ReasonTravel=="Other")
data4_visiting<-subset(data4,ReasonTravel=="Visiting")
library(ggplot2)
durations_and_reasontravel_continuous_distribution<-  ggplot()+
  geom_density(data = data4_business,aes(x=log(data4_business$nightsaway),color=("Business"))) + 
  geom_density(data = data4_holiday,aes(x=log(data4_holiday$nightsaway),color=("Holiday"))) +
  geom_density(data = data4_other,aes(x=log(data4_other$nightsaway),color=("Other"))) +
  geom_density(data = data4_visiting,aes(x=log(data4_visiting$nightsaway),color=("Visiting"))) +
  scale_colour_manual( name="",
                       values = c("red", "blue", "grey", "green")) +
  labs(x="Travel duration (nights away)",y="Probability density",title = "Continuous distibution for duration of travel outside Swaziland \nfor different age groups") +
  theme_classic() +
  theme(legend.position = "bottom") 

###duration and border posts
data4_LavumisaGolela<-subset(data4,BorderPost=="Lavumisa/Golela")
data4_LomahashaNamaacha<-subset(data4,BorderPost=="Lomahasha/Namaacha")
data4_Mahamba<-subset(data4,BorderPost=="Mahamba")
data4_Mananga<-subset(data4,BorderPost=="Mananga")
data4_MatsamoJeppesReef<-subset(data4,BorderPost=="Matsamo/Jeppe\'s Reef")
data4_MatsaphaAirport<-subset(data4,BorderPost=="Matsapha Airport")
data4_MhlumeniGoba<-subset(data4,BorderPost=="Mhlumeni/Goba")
data4_NgwenyaOshoek<-subset(data4,BorderPost=="Ngwenya/Oshoek")
data4_SalitjeOnverwacht<-subset(data4,BorderPost=="Salitje/Onverwacht")
data4_SandlaneNerston<-subset(data4,BorderPost=="Sandlane/Nerston")
data4_SicunusaHoutkop<-subset(data4,BorderPost=="Sicunusa/Houtkop")

library(ggplot2)
duration_and_borderpost_continuous_distribution<- ggplot() +
  geom_density(data=data4_LavumisaGolela,aes(x=log(data4_LavumisaGolela$nightsaway),color="Lavumisa/Golela")) +
  geom_density(data=data4_LomahashaNamaacha,aes(x=log(data4_LomahashaNamaacha$nightsaway),color="Lomahasha/Namaacha")) +
  geom_density(data=data4_Mahamba,aes(x=log(data4_Mahamba$nightsaway),color="Mahamba")) +
  geom_density(data=data4_Mananga,aes(x=log(data4_Mananga$nightsaway),color="Manangar")) +
  geom_density(data=data4_MatsamoJeppesReef,aes(x=log(data4_MatsamoJeppesReef$nightsaway),color="Matsamo/Jeppe\'s Reef")) +
  geom_density(data=data4_MatsaphaAirport,aes(x=log(data4_MatsaphaAirport$nightsaway),color="Matsapha Airport")) +
  geom_density(data=data4_MhlumeniGoba,aes(x=log(data4_MhlumeniGoba$nightsaway),color="Mhlumeni/Goba")) +
  geom_density(data=data4_NgwenyaOshoek,aes(x=log(data4_NgwenyaOshoek$nightsaway),color="Ngwenya/Oshoek")) +
  geom_density(data=data4_SalitjeOnverwacht,aes(x=log(data4_SalitjeOnverwacht$nightsaway),color="Salitje/Onverwacht")) +
  geom_density(data=data4_SandlaneNerston,aes(x=log(data4_SandlaneNerston$nightsaway),color="Sandlane/Nerston")) +
  geom_density(data=data4_SicunusaHoutkop,aes(x=log(data4_SicunusaHoutkop$nightsaway),color="Sicunusa/Houtkop")) +
  
  scale_color_manual(name="", values=c("grey","blue","yellow","brown","black","red","orange","grey","green","beige","darkred")) +
  labs(x="Duration of travel (nights away)", y="Probability density", title="Continuous distribution of duration of travel outside Swaziland \nfor different border crossings") +
  theme_classic() +
  theme(legend.position = "bottom")




##correlations
cor.test(data4$distance_out, data4$falciparum_travel_prevalence, method="pearson",alternative = "two.sided") # r value and two tailed one sample t test to test whether value for r is significantly different to zero (meaning there is insufficient evidence to accept that there is a relationship between p falcip and distance)



# Model TRAVEL OUT --------------------------------------------------------
# ##Modelling things affecting malaria## ---------------------------------------------------

###run simple linear and logistic univariate glms###

malaria_Travel_in_out<-glm(as.factor(Malaria_0_1) ~ as.factor(Travel_in_out), data = subset(datain, Travel_in_out==1|Travel_in_out==2), family=binomial(link=logit)) 
summary(malaria_Travel_in_out)
conf.malaria_Travel_in_out<-confint(malaria_Travel_in_out, level=0.95)
exp.malaria_Travel_in_out<- exp(coef(malaria_Travel_in_out))
exp.conf.malaria_Travel_in_out<-exp(confint(malaria_Travel_in_out, level=0.95))


malaria_distance.OUT<-glm(as.factor(Malaria_0_1) ~ log(distance_out), data = data4, family=binomial(link=logit)) # malaria modelled by distance
summary(malaria_distance.OUT)
confint(malaria_distance.OUT, level=0.95)
exp.malaria_distance.OUT<- exp(coef(malaria_distance.OUT))
exp.conf.malaria_distance.OUT<-exp(confint(malaria_distance.OUT, level=0.95))

malaria_travel_prevalence.OUT<-glm(as.factor(Malaria_0_1_prevalence_exists) ~ falciparum_travel_prevalence, data = data4, family=binomial(link=logit)) # malaria modelled by travel_prevalence
summary(malaria_travel_prevalence.OUT)
conf.malaria_travel_prevalence.OUT<-confint(malaria_travel_prevalence.OUT, level=0.95)
exp.malaria_travel_prevalence.OUT<- exp(coef(malaria_travel_prevalence.OUT))
exp.conf.malaria_travel_prevalence.OUT<-exp(confint(malaria_travel_prevalence.OUT, level=0.95))

malaria_time.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(Month.travel), data = data4, family=binomial(link=logit))
summary(malaria_time.OUT)
conf.malaria_time.OUT<-confint(malaria_time.OUT, level=0.95)
exp.malaria_time.OUT<-exp(coef(malaria_time.OUT))
exp.conf.malaria_time.OUT<-exp(confint(malaria_time.OUT)) 

malaria_time_binary.OUT<-glm(as.factor(Malaria_0_1) ~ as.factor(High.risk.month), data = data4, family=binomial(link=logit))
summary(malaria_time_binary.OUT)
conf.malaria_time_binary.OUT<-confint(malaria_time_binary.OUT, level=0.95)
exp.malaria_time_binary.OUT<-exp(coef(malaria_time_binary.OUT))
exp.conf.malaria_time_binary.OUT<-exp(confint(malaria_time_binary.OUT))

malaria_country.OUT<-glm(as.factor(Malaria_0_1) ~ data4_selected_countries,data = data4, family=binomial(link=logit)) # distance modelled by occupation. nb. warnings are valid - variation between 1 and 0 for malaria only exists in Mozambique, Nigeria, South Africa and Zimbabwe
summary(malaria_country.OUT)
conf.malaria_country.OUT<-confint(malaria_country.OUT, level=0.95)
exp.malaria_country.OUT<- exp(coef(malaria_country.OUT))
exp.conf.malaria_country.OUT<-exp(confint(malaria_country.OUT, level=0.95)) 

malaria_duration.OUT<-glm(as.factor(Malaria_0_1) ~ log(nightsaway),data = data4, family=binomial(link=logit)) # distance modelled by occupation. nb. warnings are valid - variation between 1 and 0 for malaria only exists in Mozambique, Nigeria, South Africa and Zimbabwe
summary(malaria_duration.OUT)
conf.malaria_duration.OUT<-confint(malaria_duration.OUT, level=0.95)
exp.malaria_duration.OUT<- exp(coef(malaria_duration.OUT))
exp.conf.malaria_duration.OUT<-exp(confint(malaria_duration.OUT, level=0.95)) 



# #####Model things affecting things affecting malaria --------------------


distance_age.OUT<-glm(log(distance_out) ~ as.factor(age_class),data = data4) # distance modelled by age
summary(distance_age.OUT)
conf.distance_age.OUT<-confint(distance_age.OUT, level=0.95)
exp.distance_age.OUT<- exp(coef(distance_age.OUT))
exp.conf.distance_age.OUT<-exp(confint(distance_age.OUT, level=0.95))

distance_occupation.OUT<-glm(log(distance_out) ~ as.factor(data4_selected_occupations),data = data4) # distance modelled by occupation
summary(distance_occupation.OUT)
conf.distance_occupation.OUT<-confint(distance_occupation.OUT, level=0.95)
exp.distance_occupation.OUT<- exp(coef(distance_occupation.OUT))
exp.conf.distance_occupation.OUT<-exp(confint(distance_occupation.OUT, level=0.95))

distance_gender.OUT<-glm(log(distance_out) ~ as.factor(data4_selected_genders) ,data = data4) # distance modelled by occupation
summary(distance_gender.OUT)
conf.distance_gender.OUT<-confint(distance_gender.OUT, level=0.95)
exp.distance_gender.OUT<- exp(coef(distance_gender.OUT))
exp.conf.distance_gender.OUT<-exp(confint(distance_gender.OUT, level=0.95))

distance_reason_travel.OUT<-glm(log(distance_out) ~ as.factor(data4_selected_ReasonTravel),data = data4) # distance modelled by occupation
summary(distance_reason_travel.OUT)
conf.distance_reason_travel.OUT<-confint(distance_reason_travel.OUT, level=0.95)
exp.distance_reason_travel.OUT<- exp(coef(distance_reason_travel.OUT))
exp.conf.distance_reason_travel.OUT<-exp(confint(distance_reason_travel.OUT, level=0.95))

distance_means_travel.OUT<-glm(log(distance_out) ~ as.factor(data4_selected_MeansTravel),data = data4) # distance modelled by occupation
summary(distance_means_travel.OUT)
conf.distance_means_travel.OUT<-confint(distance_means_travel.OUT, level=0.95)
exp.distance_means_travel.OUT<- exp(coef(distance_means_travel.OUT))
exp.conf.distance_means_travel.OUT<-exp(confint(distance_means_travel.OUT, level=0.95))
 
distance_border_post.OUT<-glm(log(distance_out) ~ as.factor(data4_selected_BorderPost),data = data4) # distance modelled by occupation
summary(distance_border_post.OUT)
conf.distance_border_post.OUT<-confint(distance_border_post.OUT, level=0.95)
exp.distance_border_post.OUT<- exp(coef(distance_border_post.OUT))
exp.conf.distance_border_post.OUT<-exp(confint(distance_border_post.OUT, level=0.95))

distance_pop_HOME.OUT<-glm(log(distance_out) ~ data4$pop.HOME,data = data4) # distance modelled by occupation
summary(distance_pop_HOME.OUT)
conf.distance_pop_HOME.OUT<-confint(distance_pop_HOME.OUT, level=0.95)
exp.distance_pop_HOME.OUT<- exp(coef(distance_pop_HOME.OUT))
exp.conf.distance_pop_HOME.OUT<-exp(confint(distance_pop_HOME.OUT, level=0.95))

distance_HOME_urban_rural_distance.OUT<-glm(log(distance_out) ~ data4$HOME_urban_rural_distance..degrees.,data = data4) # distance modelled by occupation
summary(distance_HOME_urban_rural_distance.OUT)
conf.distance_HOME_urban_rural_distance.OUT<-confint(distance_HOME_urban_rural_distance.OUT, level=0.95)
exp.distance_HOME_urban_rural_distance.OUT<- exp(coef(distance_HOME_urban_rural_distance.OUT))
exp.conf.distance_HOME_urban_rural_distance.OUT<-exp(confint(distance_HOME_urban_rural_distance.OUT, level=0.95))

distance_time.OUT<-glm(log(distance_out) ~ as.factor(Month.travel),data = data4) # distance modelled by age
summary(distance_time.OUT)
conf.distance_time.OUT<-confint(distance_time.OUT, level=0.95)
exp.distance_time.OUT<- exp(coef(distance_time.OUT))
exp.conf.distance_time.OUT<-exp(confint(distance_time.OUT, level=0.95))







travel_prevalence_age.OUT<-glm(falciparum_travel_prevalence ~ as.factor(age_class), data = data4) 
summary(travel_prevalence_age.OUT)
conf.travel_prevalence_age.OUT<-confint(travel_prevalence_age.OUT, level=0.95)
exp.travel_prevalence_age.OUT<- exp(coef(travel_prevalence_age.OUT))
exp.conf.travel_prevalence_age.OUT<-exp(confint(travel_prevalence_age.OUT, level=0.95))

travel_prevalence_occupation.OUT<-glm(falciparum_travel_prevalence ~ as.factor(data4_selected_occupations), data = data4) 
summary(travel_prevalence_occupation.OUT)
conf.travel_prevalence_occupation.OUTconfint(travel_prevalence_occupation.OUT, level=0.95)
exp.travel_prevalence_occupation.OUT<- exp(coef(travel_prevalence_occupation.OUT))
exp.conf.travel_prevalence_occupation.OUT<-exp(confint(travel_prevalence_occupation.OUT, level=0.95))

travel_prevalence_gender.OUT<-glm(falciparum_travel_prevalence ~ as.factor(Gender), data = data4) 
summary(travel_prevalence_gender.OUT)
conf.travel_prevalence_gender.OUT<-confint(travel_prevalence_gender.OUT, level=0.95)
exp.travel_prevalence_gender.OUT<- exp(coef(travel_prevalence_gender.OUT))
exp.conf.travel_prevalence_gender.OUT<-exp(confint(travel_prevalence_gender.OUT, level=0.95))


travel_prevalence_ReasonTravel.OUT<-glm(falciparum_travel_prevalence ~ as.factor(data4_selected_ReasonTravel), data = data4) 
summary(travel_prevalence_ReasonTravel.OUT)
coef.travel_prevalence_ReasonTravel.OUT<-coef(travel_prevalence_ReasonTravel.OUT)
exp.travel_prevalence_ReasonTravel.OUT<- exp(coef(travel_prevalence_ReasonTravel.OUT))
exp.conf.travel_prevalence_ReasonTravel.OUT<-exp(confint(travel_prevalence_ReasonTravel.OUT, level=0.95))

travel_prevalence_MeansTravel.OUT<-glm(falciparum_travel_prevalence ~ as.factor(data4_selected_MeansTravel), data = data4) 
summary(travel_prevalence_MeansTravel.OUT)
conf.travel_prevalence_MeansTravel.OUT<-confint(travel_prevalence_MeansTravel.OUT, level=0.95)
exp.travel_prevalence_MeansTravel.OUT<- exp(coef(travel_prevalence_MeansTravel.OUT))
exp.conf.travel_prevalence_MeansTravel.OUT<-exp(confint(travel_prevalence_MeansTravel.OUT, level=0.95))


travel_prevalence_BorderPost.OUT<-glm(falciparum_travel_prevalence ~ as.factor(data4_selected_BorderPost), data = data4) 
travel_prevalence_BorderPost.OUT
summary(travel_prevalence_BorderPost.OUT)
coef.travel_prevalence_BorderPost.OUT<-coef(travel_prevalence_BorderPost.OUT)
coef.travel_prevalence_BorderPost.OUT
exp.travel_prevalence_BorderPost.OUT<- exp(coef(travel_prevalence_BorderPost.OUT))
exp.travel_prevalence_BorderPost.OUT
confint(travel_prevalence_BorderPost.OUT, level=0.95)
conf.travel_prevalence_BorderPost.OUT<-exp(confint(travel_prevalence_BorderPost.OUT, level=0.95))
conf.travel_prevalence_BorderPost.OUT

travel_prevalence_pop.HOME.OUT<-glm(falciparum_travel_prevalence ~ pop.HOME, data = data4) 
summary(travel_prevalence_pop.HOME.OUT)
conf.travel_prevalence_pop.HOME.OUT<-confint(travel_prevalence_pop.HOME.OUT, level=0.95)
exp.travel_prevalence_pop.HOME.OUT<- exp(coef(travel_prevalence_pop.HOME.OUT))
exp.conf.travel_prevalence_pop.HOME.OUT<-exp(confint(travel_prevalence_pop.HOME.OUT, level=0.95))


travel_prevalence_pop.HOME_urban_rural_distance.OUT<-glm(falciparum_travel_prevalence ~ HOME_urban_rural_distance..degrees., data = data4) 
summary(travel_prevalence_pop.HOME_urban_rural_distance.OUT)
conf.travel_prevalence_pop.HOME_urban_rural_distance.OUT<-confint(travel_prevalence_pop.HOME_urban_rural_distance.OUT, level=0.95)
exp.travel_prevalence_pop.HOME_urban_rural_distance.OUT<- exp(coef(travel_prevalence_pop.HOME_urban_rural_distance.OUT))
exp.conf.travel_prevalence_pop.HOME_urban_rural_distance.OUT<-exp(confint(travel_prevalence_pop.HOME_urban_rural_distance.OUT, level=0.95))


travel_prevalence_time.OUT<-glm(falciparum_travel_prevalence ~ as.factor(Month.travel), data = data4) 
summary(travel_prevalence_time.OUT)
conf.travel_prevalence_time.OUT<-confint(travel_prevalence_time.OUT, level=0.95)
exp.travel_prevalence_time.OUT<- exp(coef(travel_prevalence_time.OUT))
exp.conf.travel_prevalence_time.OUT<-exp(confint(travel_prevalence_time.OUT, level=0.95))








time_binary_age.OUT<-glm(as.factor(High.risk.month) #high risk month defined in 'excel'high risk month' column in data.csv
                  ~ as.factor(age_class), data = data4, family=binomial(link=logit))
summary(time_binary_age.OUT)
conf.time_binary_age.OUT<-confint(time_binary_age.OUT, level=0.95)
exp.time_binary_age.OUT<-exp(coef(time_binary_age.OUT))
exp.conf.time_binary_age.OUT<-exp(confint(time_binary_age.OUT))

time_binary_occupation.OUT<-glm(as.factor(High.risk.month) ~ data4_selected_occupations, data = data4, family=binomial(link=logit))
summary(time_binary_occupation.OUT)
confint.time_binary_occupation.OUT<-confint(time_binary_occupation.OUT, level=0.95)
exp.time_binary_occupation.OUT<-exp(coef(time_binary_occupation.OUT))
exp.conf.time_binary_occupation.OUT<-exp(confint(time_binary_occupation.OUT)) 

time_binary_gender.OUT<-glm(as.factor(High.risk.month)
                     ~ data4_selected_genders, data = data4, family=binomial(link=logit))
summary(time_binary_gender.OUT)
conf.time_binary_gender.OUT<-confint(time_binary_gender.OUT, level=0.95)
exp.time_binary_gender.OUT<-exp(coef(time_binary_gender.OUT))
exp.conf.time_binary_gender.OUT<-exp(confint(time_binary_gender.OUT))

time_binary_reason_travel.OUT<-glm(as.factor(High.risk.month)
                            ~ data4_selected_ReasonTravel, data = data4, family=binomial(link=logit))
summary(time_binary_reason_travel.OUT) 
conf.time_binary_reason_travel.OUT<-confint(time_binary_reason_travel.OUT, level=0.95) 
coef.time_binary_reason_travel.OUT<-coef(time_binary_reason_travel.OUT) 
exp.time_binary_reason_travel.OUT<-exp(coef(time_binary_reason_travel.OUT)) 
exp.conf.time_binary_reason_travel.OUT<-exp(confint(time_binary_reason_travel.OUT)) 

time_binary_means_travel.OUT<-glm(as.factor(High.risk.month) 
                           ~ data4_selected_MeansTravel, data = data4, family=binomial(link=logit))
summary(time_binary_means_travel.OUT) 
conf.time_binary_means_travel.OUT<-confint(time_binary_means_travel.OUT, level=0.95) 
exp.time_binary_means_travel.OUT<-exp(coef(time_binary_means_travel.OUT)) 
exp.conf.time_binary_means_travel.OUT<-exp(confint(time_binary_means_travel.OUT)) 

time_binary_pop_HOME.OUT<-glm(as.factor(High.risk.month)
                       ~ pop.HOME, data = data4, family=binomial(link=logit))
summary(time_binary_pop_HOME.OUT) 
conf.time_binary_pop_HOME.OUT<-confint(time_binary_pop_HOME.OUT, level=0.95) 
exp.time_binary_pop_HOME.OUT<-exp(coef(time_binary_pop_HOME.OUT)) 
exp.conf.time_binary_pop_HOME.OUT<-exp(confint(time_binary_pop_HOME.OUT)) 

time_binary_pop_urban_rural_distance.OUT<-glm(as.factor(High.risk.month)
                                       ~ HOME_urban_rural_distance..degrees., data = data4, family=binomial(link=logit))
summary(time_binary_pop_urban_rural_distance.OUT) 
conf.time_binary_pop_urban_rural_distance.OUT<-confint(time_binary_pop_urban_rural_distance.OUT, level=0.95) 
exp.time_binary_pop_urban_rural_distance.OUT<-exp(coef(time_binary_pop_urban_rural_distance.OUT)) 
exp.conf.time_binary_pop_urban_rural_distance.OUT<-exp(confint(time_pop_urban_rural_distance.OUT)) 

time_binary_distance.OUT<-glm(as.factor(High.risk.month)
                                              ~ distance_out, data = data4, family=binomial(link=logit))
summary(time_binary_distance.OUT)
conf.time_binary_distance.OUT<-confint(time_binary_distance.OUT, level=0.95) 
exp.time_binary_distance.OUT<-exp(coef(time_binary_distance.OUT)) 
exp.conf.time_binary_distance.OUT<-exp(confint(time_binary_distance.OUT)) 

time_binary_country.OUT<-glm(as.factor(High.risk.month)
                              ~ Country, data = data4, family=binomial(link=logit))
summary(time_binary_country.OUT) 
conf.time_binary_country.OUT<-confint(time_binary_country.OUT, level=0.95) 
exp.time_binary_country.OUT<-exp(coef(time_binary_country.OUT)) 
exp.conf.time_binary_country.OUT<-exp(confint(time_binary_country.OUT)) 

time_binary_border_post.OUT<-glm(as.factor(High.risk.month)
                                 ~ data4_selected_BorderPost, data = data4, family=binomial(link=logit))
summary(time_binary_border_post.OUT) 
conf.time_binary_border_post.OUT<-confint(time_binary_border_post.OUT, level=0.95) 
exp.time_binary_border_post.OUT<-exp(coef(time_binary_border_post.OUT)) 
exp.conf.time_binary_border_post.OUT<-exp(confint(time_binary_border_post.OUT)) 

time_binary_duration.OUT<-glm(as.factor(High.risk.month)
                                 ~ nightsaway, data = data4, family=binomial(link=logit))
summary(time_binary_duration.OUT) 
conf.time_binary_duration.OUT<-confint(time_binary_duration.OUT, level=0.95) 
exp.time_binary_duration.OUT<-exp(coef(time_binary_duration.OUT)) 
exp.conf.time_binary_duration.OUT<-exp(confint(time_binary_duration.OUT)) 









duration_age.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_age_nightsawaydata), data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_age.OUT)
conf.duration_age.OUT<-confint(duration_age.OUT, level=0.95)
exp.duration_age.OUT<-exp(coef(duration_age.OUT))
exp.conf.duration_age.OUT<-exp(confint(duration_age.OUT))

duration_occupation.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_occupations_nightsawaydata), data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_occupation.OUT)
conf.duration_occupation.OUT<-confint(duration_occupation.OUT, level=0.95)
exp.duration_occupation.OUT<-exp(coef(duration_occupation.OUT))
exp.conf.duration_occupation.OUT<-exp(confint(duration_occupation.OUT)) 

duration_gender.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_genders_nightsawaydata), data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_gender.OUT)
conf.duration_gender.OUT<-confint(duration_gender.OUT, level=0.95)
exp.duration_gender.OUT<-exp(coef(duration_gender.OUT))
exp.conf.duration_gender.OUT<-exp(confint(duration_gender.OUT))

duration_reason_travel.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_ReasonTravel_nightsawaydata), data = data4)
summary(duration_reason_travel.OUT) 
conf.duration_reason_travel.OUT<-confint(duration_reason_travel.OUT, level=0.95) 
exp.duration_reason_travel.OUT<-exp(coef(duration_reason_travel.OUT)) 
exp.conf.duration_reason_travel.OUT<-exp(confint(duration_reason_travel.OUT)) 

duration_means_travel.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_MeansTravel_nightsawaydata), data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_means_travel.OUT) 
conf.duration_means_travel.OUT<-confint(duration_means_travel.OUT, level=0.95) 
exp.duration_means_travel.OUT<-exp(coef(duration_means_travel.OUT)) 
exp.conf.duration_means_travel.OUT<-exp(confint(duration_means_travel.OUT)) 

duration_border_post.OUT<-glm(log(nightsaway) ~ as.factor(data4_selected_BorderPost_nightsawaydata), data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_border_post.OUT) 
conf.duration_border_post.OUT<-confint(duration_border_post.OUT, level=0.95) 
exp.duration_border_post.OUT<-exp(coef(duration_border_post.OUT)) 
exp.conf.duration_border_post.OUT<-exp(confint(duration_border_post.OUT)) 

duration_pop_HOME.OUT<-glm(log(nightsaway) ~ pop.HOME, data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_border_post.OUT) 
conf.duration_pop_HOME.OUT<-confint(duration_pop_HOME.OUT, level=0.95) 
exp.duration_pop_HOME.OUT<-exp(coef(duration_pop_HOME.OUT)) 
exp.conf.duration_pop_HOME.OUT<-exp(confint(duration_pop_HOME.OUT)) 

duration_pop_urban_rural_distance.OUT<-glm(log(nightsaway) ~ HOME_urban_rural_distance..degrees., data = subset(data4,nightsaway!=""&nightsaway!="-Inf"&nightsaway!="Inf"&nightsaway!="0"))
summary(duration_pop_urban_rural_distance.OUT) 
conf.duration_pop_urban_rural_distance.OUT<-confint(duration_pop_urban_rural_distance.OUT, level=0.95) 
exp.duration_pop_urban_rural_distance.OUT<-exp(coef(duration_pop_urban_rural_distance.OUT)) 
exp.conf.duration_pop_urban_rural_distance.OUT<-exp(confint(duration_pop_urban_rural_distance.OUT)) 


# ###Multiple linear and logistic regression univariate models ------------


time_binary.OUT<- glm(as.factor(High.risk.month) ~ as.factor(subset(data4_selected_occupations, is.na(Occupation_code)==FALSE)) + 
                        as.factor(subset(data4_selected_genders, is.na(Gender)==FALSE)) + 
                        as.factor(subset(data4_selected_ReasonTravel, is.na(ReasonTravel)==FALSE)) + 
                        as.factor(subset(data4_selected_MeansTravel, is.na(MeansTravel)==FALSE)),
                      data=data4, family=binomial(link=logit))
summary(time_binary.OUT) 
conf.time_binary.OUT<-confint(time_binary.OUT, level=0.95) 
exp.time_binary.OUT<-exp(coef(time_binary.OUT)) 
exp.conf.time_binary.OUT<-exp(confint(time_binary.OUT)) 


#e.g. ln(OR) of a manual labourer, male,  travelling for 'visiting' by truck = 1.548142e+00 (1) + 2.374444e+00 (1) + 2.608711e+00 (1)+ 9.271252e+05(1) + 1.760415e+00


#e.g. ln(OR) of a factory worker, female, travelling for 'other' by foot =  7.947335e-01(1) + 0 + 0 + 9.931963e-01 (1) +1.760415e+00
   
  
# ###displaying results in table ------------------------------------------
library(stargazer)

stargazer (malaria_Travel_in_out, malaria_distance.OUT, malaria_travel_prevalence.OUT, malaria_time.OUT, malaria_time_binary.OUT, malaria_country.OUT, malaria_duration.OUT, type = "html", title = "Coefficients, standard error and significance of t / z values for univariate logistic models of variables affecting malaria incidence", style = "default", 
           summary = NULL, out = "regression_table_malaria.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Malaria incidence models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           notes = "(1) malaria ~ international or domestic travel, (2) malaria ~ distance, (3) malaria ~ P. falcip prevalence, (4) malaria ~ time of year (5) malaria ~ country", notes.align = "l")
stargazer (malaria_time_binary.OUT, type = "html", title = "Coefficients, standard error and significance of t / z values for univariate logistic models of variables affecting malaria incidence", style = "default", 
           summary = NULL, out = "regression_table_malaria_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Malaria incidence models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           notes = "malaria ~ month travelled", notes.align = "l")
stargazer (malaria_Travel_in_out , type = "html", title = "Coefficients, standard error and significance of t / z values for univariate logistic models of variables affecting malaria incidence", style = "default", 
           summary = NULL, out = "regression_table_malaria_Travel_in_out.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Malaria incidence models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,
           notes = "malaria ~ Travel_in_out", notes.align = "l")



stargazer (distance_age.OUT,distance_occupation.OUT,distance_gender.OUT, distance_reason_travel.OUT, distance_means_travel.OUT, distance_border_post.OUT, distance_pop_HOME.OUT, distance_HOME_urban_rural_distance.OUT,distance_time.OUT, 
           type = "html", title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting distance", style = "default", 
           summary = NULL, out = "regression_table_distance.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Distance models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "(1) distance ~ age, (2) distance ~ occupation, (3) distance ~ gender, (4) distance ~ reason for travel (5) distance ~ travel means (6) distance ~ border post (7) distance ~ home population (8) distance ~ home distance to nearest urban centre (9) distance ~ month.travel", notes.align = "l") 
stargazer (distance_time.OUT, 
           type = "html", title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting distance", style = "default", 
           summary = NULL, out = "regression_table_distance_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec","reference category - Jan"),dep.var.caption = "Distance model",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "distance ~ month.travel", notes.align = "l") 


stargazer (travel_prevalence_age.OUT,travel_prevalence_occupation.OUT,travel_prevalence_gender.OUT, travel_prevalence_ReasonTravel.OUT, travel_prevalence_MeansTravel.OUT, 
           travel_prevalence_BorderPost.OUT, travel_prevalence_pop.HOME.OUT, travel_prevalence_pop.HOME_urban_rural_distance.OUT,travel_prevalence_time.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting P. falcip. prevalence at travel location", style = "default", 
           summary = NULL, out = "regression_table_prevalence.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "travel P. falcip. prevalence models",
           dep.var.labels = NULL, dep.var.labels.include = TRUE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "(1) travel P. falcip. prevalence ~ age, (2) travel P. falcip. prevalence ~ occupation, (3) travel P. falcip. prevalence ~ gender, (4) travel P. falcip. prevalence ~ reason for travel (5) travel P. falcip. prevalence ~ travel means (6) travel P. falcip. prevalence ~ border post (7) travel P. falcip. prevalence ~ home population (8) travel P. falcip. prevalence ~ home distance to nearest urban centre (9) travel P. falcip. prevalence ~ month.travel", notes.align = "l") 
stargazer (travel_prevalence_time.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting P. falcip. prevalence at travel location", style = "default", 
           summary = NULL, out = "regression_table_prevalence_time.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec","reference category - Jan"), dep.var.caption = "travel P. falcip. prevalence model",
           dep.var.labels = NULL, dep.var.labels.include = TRUE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "travel P. falcip. prevalence ~ month.travel", notes.align = "l") 





stargazer (time_binary_age.OUT,time_binary_occupation.OUT,time_binary_gender.OUT,time_binary_reason_travel.OUT, time_binary_means_travel.OUT, 
           time_binary_border_post.OUT, time_binary_pop_HOME.OUT, time_binary_pop_urban_rural_distance.OUT,time_binary_distance.OUT,time_binary_duration.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting month travelled (high vs. low risk)", style = "default", 
           summary = NULL, out = "regression_table_time_binary.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "High vs. low risk month travel models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "(1) High.risk.month ~ age, (2) High.risk.month ~ occupation, (3) High.risk.month ~ gender, (4) High.risk.month ~ reason for travel (5) High.risk.month ~ travel means (6) High.risk.month ~ border post (7) High.risk.month ~ home population (8) High.risk.month ~ home distance to nearest urban centre", notes.align = "l")
stargazer (time_binary_occupation.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting month travelled (high vs. low risk)", style = "default", 
           summary = NULL, out = "regression_table_time_binary_occupation.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("farming / agriculture", "manufacturing / factory", "office / clerical work", "other", "other manual labour", "student",  "reference category - unemployed"), dep.var.caption = "High vs. low risk month travel model",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "High.risk.month ~ occupation", notes.align = "l")
stargazer (time_binary_gender.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting month travelled (high vs. low risk)", style = "default", 
           summary = NULL, out = "regression_table_time_binary_gender.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("male", "reference category - female"), dep.var.caption = "High vs. low risk month travel model",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "High.risk.month ~ gender", notes.align = "l")
stargazer (time_binary_reason_travel.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting month travelled (high vs. low risk)", style = "default", 
           summary = NULL, out = "regression_table_time_binary_reason_travel.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("holiday","other","visiting", "reference category - business"), dep.var.caption = "High vs. low risk month travel model",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "High.risk.month ~ reason_travel", notes.align = "l")
stargazer (time_binary_border_post.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting month travelled (high vs. low risk)", style = "default", 
           summary = NULL, out = "regression_table_time_binary_border_post.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = c("Lomahasha/Namaacha","Mahamba","Mananga","Matsamo/Jeppe's Reef", "Matsapha Airport", "Mhlumeni/Goba", "Ngwenya/Oshoek", "Other ", "Salitje/Onverwacht", "Sandlane/Nerston   ", "Sicunusa/Houtkop" , "reference category - Lavumisa/Golela"), dep.var.caption = "High vs. low risk month travel model",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "High.risk.month ~ border_post", notes.align = "l")

stargazer (duration_age.OUT,duration_occupation.OUT,duration_gender.OUT,duration_reason_travel.OUT, duration_means_travel.OUT, 
           duration_border_post.OUT, duration_pop_HOME.OUT, duration_pop_urban_rural_distance.OUT,   type = "html", 
           title = "Coefficients standard error and significance of t / z values for univariate logistic models of variables affecting travel duration", style = "default", 
           summary = NULL, out = "regression_table_duration.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Travel duration models",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "(1) travel duration ~ age, (2) travel duration ~ occupation, (3) travel duration ~ gender, (4) travel duration ~ reason for travel (5) travel duration ~ travel means (6) travel duration ~ border post (7) travel duration ~ home population (8) travel duration ~ home distance to nearest urban centre", notes.align = "l")

stargazer (time_binary.OUT,  type = "html", 
           title = "Coefficients standard error and significance of t / z values for multimple linear and logistic models of variables affecting distance, travel P. falcip prevalence and month of travel", style = "default", 
           summary = NULL, out = "regression_table_multiple.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "Dependent variable",
           dep.var.labels = c("distance", "travel P.falcip. prevalence", "month of travel"), dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "(1) high risk month ~ occupation + gender + travel purpose + mode of transport", notes.align = "l")


stargazer (data4[c("Malaria_0_1","Travel_in_out","Country","nightsaway", "High.risk.month", "Year.travel","ReasonTravel","MeansTravel",
          "BorderPost","Age","Gender", "Occupation", "Occupation_code", "HOME_urban_rural_distance..degrees.",
          "pop.HOME", "falciparum_travel_prevalence","Lon_EA_HOME","lat_EA_HOME","lon_to", "lat_to", "distance_out")], type = "html", 
          title = "Summary statistics for all individuals surveyed who travelled outside of Swaziland", style = "default", 
          summary.stat =  c("n","mean","median","max","min","sd"), out = "summary_stats_data4.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "", notes.align = "l")
stargazer (data4_malarianegative[c("Malaria_0_1","TYPE","Travel_in_out","Country","nightsaway", 
                   "Month.travel", "High.risk.month",  "Year.travel","ReasonTravel","MeansTravel",
                   "BorderPost","Age","age_class","Gender", "Occupation", "Occupation_code", "HOME_urban_rural_distance..degrees.",
                   "pop.HOME", "falciparum_travel_prevalence","Lon_EA_HOME","lat_EA_HOME","lon_to", "lat_to", "distance_out")], type = "html", 
           title = "Summary statistics for all malaria negative individuals surveyed who travelled outside of Swaziland, Malaria negative", style = "default", 
           summary.stat =  c("n","mean","median","max","min","sd"), out = "summary_stats_data4_malarianegative.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "", notes.align = "l")
stargazer (data4_malariapositive[c("Malaria_0_1","TYPE","Travel_in_out","Country","nightsaway", 
                                   "Month.travel", "High.risk.month",  "Year.travel","ReasonTravel","MeansTravel",
                                   "BorderPost","Age","age_class","Gender", "Occupation", "Occupation_code", "HOME_urban_rural_distance..degrees.",
                                   "pop.HOME", "falciparum_travel_prevalence","Lon_EA_HOME","lat_EA_HOME","lon_to", "lat_to", "distance_out")], type = "html", 
           title = "Summary statistics for all malaria positive individuals surveyed who travelled outside of Swaziland, malaria positive", style = "default", 
           summary.stat =  c("n","mean","median","max","min","sd"), out = "summary_stats_data4_malariapostitive.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "", notes.align = "l")
stargazer (subset(data4,High.risk.month==1)[c("Malaria_0_1","TYPE","Travel_in_out","Country","nightsaway", 
                                   "Month.travel", "High.risk.month",  "Year.travel","ReasonTravel","MeansTravel",
                                   "BorderPost","Age","age_class","Gender", "Occupation", "Occupation_code", "HOME_urban_rural_distance..degrees.",
                                   "pop.HOME", "falciparum_travel_prevalence","Lon_EA_HOME","lat_EA_HOME","lon_to", "lat_to", "distance_out")], type = "html", 
           title = "Summary statistics for all individuals surveyed who travelled outside of Swaziland during a high risk month", style = "default", 
           summary.stat =  c("n","mean","median","max","min","sd"), out = "summary_stats_data4_high_risk.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "", notes.align = "l")
stargazer (subset(data4,High.risk.month==0)[c("Malaria_0_1","TYPE","Travel_in_out","Country","nightsaway", 
                                              "Month.travel", "High.risk.month", "Year.travel","ReasonTravel","MeansTravel",
                                              "BorderPost","Age","age_class","Gender", "Occupation", "Occupation_code", "HOME_urban_rural_distance..degrees.",
                                              "pop.HOME", "falciparum_travel_prevalence","Lon_EA_HOME","lat_EA_HOME","lon_to", "lat_to", "distance_out")], type = "html", 
           title = "Summary statistics for all individuals surveyed who travelled outside of Swaziland during a low risk month", style = "default", 
           summary.stat =  c("n","mean","median","max","min","sd"), out = "summary_stats_data4_low_risk.html", out.header = FALSE,
           column.labels = NULL, column.separate = NULL,
           covariate.labels = NULL, dep.var.caption = "",
           dep.var.labels = NULL, dep.var.labels.include = FALSE,star.cutoffs = c( 0.05, 0.01, 0.001),
           notes = "", notes.align = "l")

####plotting results -----------------------------------------------------


####plotting univariate models (manual)
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


####plotting all models (automatic)

plot(malaria_Travel_in_out)
plot(malaria_distance.OUT)
plot(malaria_travel_prevalence.OUT)
plot(malaria_time_binary.OUT)
plot(malaria_country.OUT)
plot(malaria_duration.OUT)
plot(distance_age.OUT)
plot(distance_occupation.OUT)
plot(distance_gender.OUT)
plot(distance_reason_travel.OUT)
plot(distance_means_travel.OUT)
plot(distance_border_post.OUT)
plot(distance_pop_HOME.OUT)
plot(distance_HOME_urban_rural_distance.OUT)
plot(distance_time.OUT)
plot(travel_prevalence_age.OUT)
plot(travel_prevalence_occupation.OUT)
plot(travel_prevalence_gender.OUT)
plot(travel_prevalence_ReasonTravel.OUT)
plot(travel_prevalence_MeansTravel.OUT)
plot(travel_prevalence_BorderPost.OUT)
plot(travel_prevalence_pop.HOME.OUT)
plot(travel_prevalence_pop.HOME_urban_rural_distance.OUT)
plot(time_binary_age.OUT)
plot(time_binary_occupation.OUT)
plot(time_binary_gender.OUT)
plot(time_binary_reason_travel.OUT)
plot(time_binary_means_travel.OUT)
plot(time_binary_border_post.OUT)
plot(time_binary_pop_HOME.OUT)
plot(time_binary_pop_urban_rural_distance.OUT)
plot(time_age.OUT)
plot(time_occupation.OUT)
plot(time_gender.OUT)
plot(time_reason_travel.OUT)
plot(time_means_travel.OUT)
plot(time_border_post.OUT)
plot(time_pop_HOME.OUT)
plot(time_pop_urban_rural_distance.OUT)
plot(duration_age.OUT)
plot(duration_occupation.OUT)
plot(duration_gender.OUT)
plot(duration_reason_travel.OUT)
plot(duration_means_travel.OUT)
plot(duration_border_post.OUT)
plot(duration_pop_HOME.OUT)
plot(duration_pop_urban_rural_distance.OUT)
plot(distance.OUT)
plot(travel_prevalence.OUT)
plot(time_binary.OUT)
plot(duration.OUT)



# Test if variables meet model assumptions -----------------------------------------------------
##for multiple linear regression models

##1 linear relationship between independent and dependent variables (or log(odds) for logistic regression)
### check QQ plots
###(ignore for categorical variables) 

###malaria (plot of log(odds) against each variable for raw data)
plot(log())
###distance
boxplot(data4$age_class,log(data4$distance_out))
quantile(data4$age_class, probs=seq(0,1,0.2), na.rm=TRUE) # value below which these percentages of values fall - 0%, 20%, 40%, 60%, 80%, 100%
plot(data4$Occupation,log(data4$distance_out))#ignore 
plot(data4$Gender,log(data4$distance_out))#ignore
plot(data4$ReasonTravel,log(data4$distance_out))#ignore
plot(data4$MeansTravel,log(data4$distance_out))#ignore
plot(data4$pop.HOME,log(data4$distance_out))
plot(data4$distance_urban_rural_km,log(data4$distance_out))
cor(subset(data4, distance_out!="")$pop.HOME,subset(data4, distance_out!="")$distance_out, method="pearson")
cor.test(subset(data4, distance_out!="")$pop.HOME,subset(data4, distance_out!="")$distance_out, method="pearson")
cor(subset(data4, distance_out!="")$distance_urban_rural_km,subset(data4, distance_out!="")$distance_out, method="pearson")
cor.test(subset(data4, distance_out!="")$distance_urban_rural_km,subset(data4, distance_out!="")$distance_out, method="pearson")


test<-c(1,2,3,6,7,8,10,11,15,16,17)

###prevalence
plot(data4$Age,data4$falciparum_travel_prevalence)#ignore
plot(data4$Occupation,data4$falciparum_travel_prevalence)#ignore
plot(data4$Gender,data4$falciparum_travel_prevalence)#ignore
plot(data4$ReasonTravel,data4$falciparum_travel_prevalence)#ignore
plot(data4$MeansTravel,data4$falciparum_travel_prevalence)#ignore
plot(data4$pop.HOME,data4$falciparum_travel_prevalence)
plot(data4$distance_urban_rural_km,data4$falciparum_travel_prevalence)
cor(subset(data4, falciparum_travel_prevalence!="")$pop.HOME,subset(data4, falciparum_travel_prevalence!="")$falciparum_travel_prevalence, method="pearson")
cor.test(subset(data4, falciparum_travel_prevalence!="")$pop.HOME,subset(data4, falciparum_travel_prevalence!="")$falciparum_travel_prevalence, method="pearson")
cor(subset(data4, falciparum_travel_prevalence!="")$distance_urban_rural_km,subset(data4, falciparum_travel_prevalence!="")$falciparum_travel_prevalence, method="pearson")
cor.test(subset(data4, falciparum_travel_prevalence!="")$distance_urban_rural_km,subset(data4, falciparum_travel_prevalence!="")$falciparum_travel_prevalence, method="pearson")


###duration
plot(data4$age_class,log(data4$nightsaway))#ignore
plot(data4$Occupation,log(data4$nightsaway))#ignore
plot(data4$Gender,log(data4$nightsaway))#ignore
plot(data4$ReasonTravel,log(data4$nightsaway))#ignore
plot(data4$MeansTravel,log(data4$nightsaway))#ignore
plot(data4$pop.HOME,log(data4$nightsaway))
plot(data4$distance_urban_rural_km,log(data4$nightsaway))
cor(subset(data4, nightsaway!="")$pop.HOME,subset(data4, nightsaway!="")$nightsaway, method="pearson")
cor.test(subset(data4, nightsaway!="")$pop.HOME,subset(data4, nightsaway!="")$nightsaway, method="pearson")
cor(subset(data4, nightsaway!="")$distance_urban_rural_km,subset(data4, nightsaway!="")$nightsaway, method="pearson")
cor.test(subset(data4, nightsaway!="")$distance_urban_rural_km,subset(data4, nightsaway!="")$nightsaway, method="pearson")


###month.away #ignore


##2 little or no multicollinearity between independent variables

library(car)
vif(distance.OUT) # variance inflation factors . Remove data4_selected_MeansTravel (28.945339) or data4_selected_BorderPost (44.288373) xx 
sqrt(vif(distance.OUT))
vif(travel_prevalence.OUT) # variance inflation factors . Removed none. 
sqrt(vif(travel_prevalence.OUT))
vif(time_binary.OUT) # variance inflation factors 
sqrt(vif(time_binary.OUT))
vif(duration.OUT) # variance inflation factors . Removed none. 
sqrt(vif(duration.OUT))
#######As a rule of thumb, if the VIF of a variable exceeds 10, which will happen if multiple correlation coefficient for j-th variable R2jRj2 exceeds 0.90, that variable is said to be highly collinear.



##3 all variables have normal distribution AND all there is a multivariate normal distribution (i.e. model residuals are normally distributed)
hist(log(data4$distance_out),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(data4$distance_out)))
hist(log(data4$distance_out),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(distance_out)))## use log(distance)

hist((data4$falciparum_travel_prevalence),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=falciparum_travel_prevalence)) 

hist(log(data4$nightsaway),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(data4$nightsaway))) 
hist(log(data4$nightsaway),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(nightsaway))) ##use log(nightsaway)

hist((data4$age_class),breaks = 40, freq = FALSE)

hist((data4$pop.HOME),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=pop.HOME)) 
hist(log(data4$pop.HOME),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(pop.HOME))) ##use log(pop.HOME)

hist((data4$distance_urban_rural_km),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=distance_urban_rural_km)) 
hist(log(data4$distance_urban_rural_km),breaks = 40, freq = FALSE)
ggplot()+geom_density(data = data4 , aes(x=log(distance_urban_rural_km))) ## xx bimodal - remove? no. check there are at least a few in each category

##3.2 model residuals are normally distributed - see normal qqplots 





##4 variables are homoscedastic #see first plotted graph for each model and look to see if variance is constant
######Breusch-Pagan test, null hypothesis is that the variance of the residuals is constant
lmtest::bptest(malaria_Travel_in_out) heteroscedastic 
lmtest::bptest(malaria_distance.OUT) heteroscedastic 
lmtest::bptest(malaria_travel_prevalence.OUT)
lmtest::bptest(malaria_time_binary.OUT)
lmtest::bptest(malaria_country.OUT)
lmtest::bptest(malaria_duration.OUT)
lmtest::bptest(distance_age.OUT)
lmtest::bptest(distance_occupation.OUT)
lmtest::bptest(distance_gender.OUT)
lmtest::bptest(distance_reason_travel.OUT)
lmtest::bptest(distance_means_travel.OUT)
lmtest::bptest(distance_border_post.OUT)
lmtest::bptest(distance_pop_HOME.OUT)
lmtest::bptest(distance_HOME_urban_rural_distance.OUT)
lmtest::bptest(distance_time.OUT)
lmtest::bptest(travel_prevalence_age.OUT)
lmtest::bptest(travel_prevalence_occupation.OUT)
lmtest::bptest(travel_prevalence_gender.OUT)
lmtest::bptest(travel_prevalence_ReasonTravel.OUT)
lmtest::bptest(travel_prevalence_MeansTravel.OUT)
lmtest::bptest(travel_prevalence_BorderPost.OUT)
lmtest::bptest(travel_prevalence_pop.HOME.OUT)
lmtest::bptest(travel_prevalence_pop.HOME_urban_rural_distance.OUT)
lmtest::bptest(time_age.OUT)
lmtest::bptest(time_occupation.OUT)
lmtest::bptest(time_gender.OUT)
lmtest::bptest(time_reason_travel.OUT)
lmtest::bptest(time_means_travel.OUT)
lmtest::bptest(time_border_post.OUT)
lmtest::bptest(time_pop_HOME.OUT)
lmtest::bptest(time_pop_urban_rural_distance.OUT)
lmtest::bptest(duration_age.OUT)
lmtest::bptest(duration_occupation.OUT)
lmtest::bptest(duration_gender.OUT)
lmtest::bptest(duration_reason_travel.OUT)
lmtest::bptest(duration_means_travel.OUT)
lmtest::bptest(duration_border_post.OUT)
lmtest::bptest(duration_pop_HOME.OUT)
lmtest::bptest(duration_pop_urban_rural_distance.OUT)
lmtest::bptest(distance.OUT)
lmtest::bptest(travel_prevalence.OUT)
lmtest::bptest(time_binary.OUT)
lmtest::bptest(duration.OUT)


##5 little or no autocorrelation in data
library(ggplot2)
data(data4)
acf(distance.OUT$residuals) 


###prevalence
###duration

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

xseq<-seq(min(log(distance_out), na.rm =TRUE),max(log(distance_out),na.rm = TRUE),0.01)
densities<-dnorm(xseq,mean(log(distance_out), na.rm =TRUE),sd(log(distance_out), na.rm = TRUE))
plot(xseq, densities, col="darkgreen",xlab="Sequence in distance range", ylab="Probability density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'Distance_out' sample size
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)

##Falciparum_travel_prevalence##

hist((data4$falciparum_travel_prevalence),10,freq=FALSE,	main="Histogram of log(distance_out) to show normal distribution",col="lightcoral");

xseq<-seq(from=min(data4$falciparum_travel_prevalence,na.rm=TRUE),to=max(data4$falciparum_travel_prevalence,na.rm=TRUE),0.01)
densities<-dnorm(xseq,mean(data4$falciparum_travel_prevalence,na.rm=TRUE),sd(data4$falciparum_travel_prevalence,na.rm=TRUE))
plot(xseq, densities, col="darkgreen",xlab="Sequence in travel_prevalence range", ylab="Probability density", type="l",lwd=2, cex=0.5, main="Normal curve with parameters estimated from 'travel_prevalence' sample size 
     (i.e. a perfect normal distribution for this sample)", cex.axis=.7,cex.main=0.7,cex.lab=0.7)








# Maps --------------------------------------------------------------------

library(ggmap)
#locations<-geocode(location=c("Golela Border, Swaziland",
 #                             "Lomahasha Border, Swaziland", 
  #                            "Mpumalanga Border, South Africa",
   #                           "Mananga, 1354, South Africa",
    #                          "Jeppes Reef, South Africa",
     #                         "Matsapha Airport, Swaziland",
      #                        "Mhlumeni Boder Post, Swaziland",
       #                       "N17, Oshoek, 2350, South Africa",
        #                      "Onverwacht Border Post, South Africa",
         #                     "Sandlane Border, South Africa",
          #                    "Sicunusa Border post, Swaziland"),output="latlon", source="google") #get latlong table (but wrong coordinates) - best way to do this in future is to use online geocoding software
data4_BorderPost_selected<-subset(data4, BorderPost  %in% c("Lavumisa/Golela"     , "Lomahasha/Namaacha",   "Mahamba"     ,         "Mananga"      ,       
                                                    "Matsamo/Jeppe's Reef", "Matsapha Airport" ,    "Mhlumeni/Goba"   ,     "Ngwenya/Oshoek" ,                    
                                                    "Salitje/Onverwacht" ,  "Sandlane/Nerston" ,    "Sicunusa/Houtkop")) # select only BorderPosts not 'other' or '""'
Borderlat<-c( -25.98955, -27.31917, -27.10524, 	-25.93233, -25.75033, -26.52033, -26.25455, -26.21268, 	-27.3175, -26.57239, -26.8605)
Borderlon<-c(31.99716,  31.89083, 31.06856, 31.76154,31.46864, 31.31413, 32.09001,30.98859, 31.64389,30.79272, 30.9084)
#these lat longs are copied from google maps and are correct 

table(data4_BorderPost_selected$BorderPost)# shows borderpost frequency table


BorderPostTable<-aggregate(data4_BorderPost_selected[,c("nightsaway", "Age")], by=list("Border_Post"=data4_BorderPost_selected$BorderPost), FUN= "mean", na.rm=TRUE)
####add comma inside list to add another variable 
BorderPostTable<-data.frame(BorderPostTable, Borderlon, Borderlat)

####use dyplr mutate function to do data frame calculations 








library(ggmap)
zambia_coordinates <- c(lon = 31.8, lat = -22)
import_map<-get_map(location=zambia_coordinates,zoom=5, maptype = "roadmap", source = "google")

BorderPost_map<-ggmap(import_map) + 
  geom_point(data = BorderPostTable, aes(x=Borderlon,y=Borderlat)) 
BorderPost_duration_means_map<-ggmap(import_map, base_layer = ggplot(BorderPostTable, aes(x=Borderlon,y=Borderlat))) + 
  geom_point() + #stop here to get map of border posts only 
  geom_point(aes(size=BorderPostTable$nightsaway)) #this gives map of border posts with size adjusted for duration 

BorderPost_high_vs_low_risk_month_map<-ggmap(import_map, base_layer = ggplot(data4, aes(x=lon_to , y=lat_to))) +
  geom_point(data=subset(data4, High.risk.month==1), color="red", size=0.5) +
  geom_point(data=subset(data4, High.risk.month==0), color="blue", size=0.5) 
BorderPost_high_vs_low_risk_month_map

BorderPost_high_risk_month_map<-ggmap(import_map, base_layer = ggplot(data4, aes(x=lon_to , y=lat_to))) +
  geom_point(data=subset(data4, High.risk.month==1), color="red", size=0.5) 
BorderPost_high_risk_month_map

BorderPost_low_risk_month_map<-ggmap(import_map, base_layer = ggplot(data4, aes(x=lon_to , y=lat_to))) +
  geom_point(data=subset(data4, High.risk.month==0), color="blue", size=0.5) 
BorderPost_low_risk_month_map

falcip_binomial_map<-ggmap(import_map, base_layer = ggplot( data4, aes(x=lon_to , y=lat_to))) +
                                geom_point(data=subset(data4,falciparum_travel_prevalence>0.07&falciparum_travel_prevalence<0.1), color="red", size=0.5)+
                                geom_point(data=subset(data4,falciparum_travel_prevalence>0.17&falciparum_travel_prevalence<0.25), color="blue", size=0.5)
  


install.packages("tiff")
library(tiff)
readTIFF(source="2015_Nature_Africa_PR.2012.tif", native = TRUE)

qmap("SO51 6RN", zoom=15, maptype="satellite")

# Outputs -----------------------------------------------------------------





##csvs for arcGIS occupation map
write.csv(data4,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4.csv")
write.csv(data4_occupation_code1,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code1.csv")
write.csv(data4_occupation_code2,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code2.csv")
write.csv(data4_occupation_code3,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code3.csv")
write.csv(data4_occupation_code4,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code4.csv")
write.csv(data4_occupation_code5,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code5.csv")
write.csv(data4_occupation_code6,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code6.csv")
write.csv(data4_occupation_code7,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code7.csv")
write.csv(data4_occupation_code8,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code8.csv")
write.csv(data4_occupation_code9,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code9.csv")
write.csv(data4_occupation_code10,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_code10.csv")
write.csv(data4_occupation_codeNA,"/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18/data4_occupation_codeNA.csv")

##plots for descriptive stats
ggsave("barchart_other_occupations_stacked.png", plot=barchart_other_occupations_stacked,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("pie_other_occupations.png",plot = pie_other_occupations,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("other_manual_labourer_seasonal_travel_frequency.png", plot=other_manual_labourer_seasonal_travel_frequency, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distribution of distances travelled outside Swaziland for different occupation groups.png", plot=distribution_of_distances_travelled_outside_Swaziland_for_different_occupation_groups, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("month_vs_means_travel_frequency.png",plot=month_vs_means_travel_frequency, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("month_vs_home_urban_rural_distance_scatter.png", plot=month_vs_home_urban_rural_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distances_and_age_class_continuous_distribution.png",plot = distances_and_age_class_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distances_and_gender_continuous_distribution.png", plot=distances_and_gender_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_meanstravel_continuous_distribution.png", plot=distance_and_meanstravel_continuous_distribution, path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_borderpost_continuous_distribution.png",plot = distance_and_borderpost_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("pop.HOME_vs_distance_scatter.png",plot=pop.HOME_vs_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("HOME_rural_urban_distance_vs_distance_scatter.png",plot=HOME_rural_urban_distance_vs_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_monthtravel_continuous_distribution.png",plot=distance_and_monthtravel_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("durations_and_age_class_continuous_distribution.png",plot=durations_and_age_class_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("durations_and_reasontravel_continuous_distribution.png",plot=durations_and_reasontravel_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("duration_and_borderpost_continuous_distribution.png",plot=duration_and_borderpost_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("malaria_cases_each_travel_month.png",plot=malaria_cases_each_travel_month,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("malaria_cases_each_travel_firstentered.png",plot=malaria_cases_each_travel_firstentered,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("malaria_cases_2010_2014.png",plot=malaria_cases_2010_2014,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_time_binary_continuous_distribution.png",plot=distance_and_time_binary_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("distance_and_time_binary_continuous_distribution_histogram.png",plot=distance_and_time_binary_continuous_distribution_histogram,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("prevalence_and_time_binary_continuous_distribution.png",plot=prevalence_and_time_binary_continuous_distribution,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("prevalence_and_time_binary_continuous_distribution_histogram.png",plot=prevalence_and_time_binary_continuous_distribution_histogram,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("histogram_cases_per_travel_month_datain.png",plot=histogram_cases_per_travel_month_datain,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("histogram_cases_per_firstentered_month_datain.png",plot=histogram_cases_per_firstentered_month_datain,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("cases_each_travel_month.png",plot=cases_each_travel_month,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("reasontravel_cases_each_travel_month.png",plot=reasontravel_cases_each_travel_month,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")









write.table(age_class_high_risk_months_table, "age_class_high_risk_months_table.txt", sep="\t")
write.table(age_class_low_risk_months_table, "age_class_low_risk_months_table.txt", sep="\t")

ggsave("High_risk_month_vs_Mhlumeni_Goba_travel_frequency.png", plot=High_risk_month_vs_Mhlumeni_Goba_travel_frequency,  path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("High_risk_month_vs_non_Mhlumeni_Goba_travel_frequency.png", plot=High_risk_month_vs_non_Mhlumeni_Goba_travel_frequency,  path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")

##plots for graphs of model outputs 
ggsave("high_risk_month_vs_rural_urban_distance_scatter.png", plot=high_risk_month_vs_rural_urban_distance_scatter,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")
ggsave("high_risk_month_vs_rural_urban_distance_function.png", plot=high_risk_month_vs_rural_urban_distance_function,path = "/Users/katiehickson/Library/Mobile Documents/com~apple~CloudDocs/Katie files 01-03-18.16/3. Exectution/Flowminder Internship 2017-18/CHAI Malaria Elimination Research/Analysis/Code and Data 2012-2014/CHAI Malaria 20-03-18")











