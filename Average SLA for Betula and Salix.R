#SC 6/20/19
#this script is for finding the average SLA values from a data sheet

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

data<-read.csv("Betula_Salix data from SLA_sfarmer and SLA_2017.csv")

#starting with an overall for betula and salix
sla<-aggregate(data$SLA..m2.g., by=list(data$Species), FUN=ave)

#betula overall SLA=0.01338274
#salix overall SLA=0.01304846

#but need to calculate average for high/low density, as well as sun/shade in these sites
#so because i don't know any other way for now let's split by species
bdata<-data[1:25,]
sdata<-data[26:50,]

#and aggregate
bsla<-aggregate(bdata$SLA..m2.g., by=list(bdata$Site), FUN=ave)
ssla<-aggregate(sdata$SLA..m2.g., by=list(sdata$Site), FUN=ave)

#Betula High Density= 0.0167443834166667
#Betula Low Density= 0.0102796925384615
#Salix High Density= 0.01454800675
#Salix Low Density= 0.0116642552307692

#now finding for sun/shade within each density site
#no data values for low density shade, so only high density sun/shade values are calculated
bhigh<-bdata[1:12,]
shigh<-sdata[1:12,]

bsunsla<-aggregate(bhigh$SLA..m2.g., by=list(bhigh$Sun.Shade), FUN=ave)
ssunsla<-aggregate(shigh$SLA..m2.g., by=list(shigh$Sun.Shade), FUN=ave)

#Betula High Density Sun= 0.01409087
#Betula High Density Shade= 0.01939790
#Salix High Density Sun= 0.01409602
#Salix High Density Shade= 0.01499999
#so since no sun vs. shade data for low density:
#If Betula Low Density, SLA= 0.0102796925384615
#If Salix Low Density, SLA= 0.0116642552307692