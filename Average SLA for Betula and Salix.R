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

