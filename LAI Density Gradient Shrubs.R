#SC 6/19/19
#script to calculate LAI for siberia density gradient shrubs

#the first part of this script was taken from another script 
#i made called "Average SLA for Betula and Salix"
#i have included it here to have all the calculations
#for the SLA values to be used in one place

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

data<-read.csv("Betula_Salix data from SLA_sfarmer and SLA_2017.csv")

#split by species
bdata<-data[1:25,]
sdata<-data[26:50,]

#and aggregate each to find the average per site
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

#Summary:
#Betula High Density Sun= 0.01409087
#Betula High Density Shade= 0.01939790
#Salix High Density Sun= 0.01409602
#Salix High Density Shade= 0.01499999
#so since no sun vs. shade data for low density:
#If Betula Low Density, SLA= 0.0102796925384615
#If Salix Low Density, SLA= 0.0116642552307692

#with SLA values calculated, now we can begin working with the shrub data:

shrub_data<-read.csv("2012 - 2017 Density Gradient Shrubs.csv")[,c(1:5,8)]

#add a column for density
#using "L" for low density and "H" for high and medium density
shrub_data$density <- ifelse(substr(shrub_data$Site,1,1) == "L",
                         "L","H")

#split by density
by_dens<-arrange(shrub_data, shrub_data$density)
hdens<-by_dens[1:2391,]
ldens<-by_dens[2392:4958,]

#split by species
hdens<-arrange(hdens, hdens$Species)
ldens<-arrange(ldens, ldens$Species)

bhdens<-hdens[1:2129,]
shdens<-hdens[2130:2391,]

bldens<-ldens[1:2353,]
sldens<-ldens[2354:2567,]

#now that they're separated we can use the appropriate SLA value
#medium density sites will be calculated with high density SLA
#also, to start I will assume sun for all calculations
#later on I may try with shade value to see if there's a significant difference

bhdens$leaf_area<-bhdens$New.Growth.Bio..g.dry.wt.*0.01409087
bldens$leaf_area<-bldens$New.Growth.Bio..g.dry.wt.*0.0102796925384615
shdens$leaf_area<-shdens$New.Growth.Bio..g.dry.wt.*0.01409602
sldens$leaf_area<-sldens$New.Growth.Bio..g.dry.wt.*0.0116642552307692

#now i'm going to full join the tables to sum up leaf area for each site
b<-full_join(bhdens,bldens)
s<-full_join(shdens,sldens)
leafarea<-full_join(b,s)

#find the sum of the leaf area in each site/plot
site_lai<-aggregate(leafarea$leaf_area, 
                    by=list(leafarea$Site, leafarea$Plot,
                            leafarea$Area.Sampled..m2.), FUN=sum)
colnames(site_lai)<-c("Site", "Plot", "Plot Area", "Total Leaf Area")

#calculate LAI
site_lai$LAI<-site_lai$`Total Leaf Area`/site_lai$`Plot Area`

#find the average value for each site
average<-aggregate(site_lai$LAI, by=list(site_lai$Site), FUN="mean")
colnames(average)<-c("Site","Average LAI")

#find the standard deviation for each site
sd<-aggregate(site_lai$LAI, by=list(site_lai$Site), FUN=sd)
colnames(sd)<-c("Site","Standard Deviation")

#i combined them into one chart here
lai<-full_join(average,sd)

write.csv(lai,"Average LAI for Density Gradient Shrubs")


#IN THE REMAINDER OF THIS CODE: various other things comparing tree and shrub data
#much of this assumes the script for LAI Density Gradient Trees has already been run
#or at least that you have the csv files

#making a bar graph to compare trees and shrubs
trees<-read.csv("Average LAI for Density Gradient Trees") [,2:3]
colnames(trees)<-c("Site", "Average Tree LAI")

shrubs<-read.csv("Average LAI for Density Gradient Shrubs") [,2:3]
colnames(shrubs)<-c("Site", "Average Shrub LAI")

raw_data<-full_join(trees, shrubs)[c(1:3,5:15,17:25),]

data<-t(raw_data[,2:3])
colnames(data)<-raw_data$Site
barplot(data, xlab="Site", ylab="Average LAI", 
        main="Average LAI of Density Gradient Trees and Shrubs",
        col=c("palegreen4", "chocolate4"),legend=rownames(data),beside=TRUE)


#comparing % betula vs salix in each shrub site
#NOTE: i am probably doing this in a weird/wrong/roundabout way.
#finding the leaf area of each species
stand_area1<-aggregate(leafarea$leaf_area, 
                    by=list(leafarea$Site, leafarea$Plot,
                            leafarea$Area.Sampled..m2., leafarea$Species), FUN=sum)
colnames(stand_area)<-c("Site", "Plot", "Plot Area", "Species", "Total Leaf Area")

#split into betula/salix
bstand_area<-stand_area[1:72,]
sstand_area<-stand_area[73:131,]

#join them into a new table to compare species on each site
#each species isn't in each plot, so i'll have to make sure to deal with NA values
species<-full_join(bstand_area,sstand_area, by=c("Site","Plot","Plot Area"))
colnames(species)<-c("Site","Plot","Plot Area","Betula","Betula Area","Salix","Salix Area")

#making the NA values equal zero
species$`Salix Area`[is.na(species$`Salix Area`)]<-0
species$`Betula Area`[is.na(species$`Betula Area`)]<-0

#adding betula and salix total area
species$'B+S'<-species$`Betula Area`+species$`Salix Area`

#dividing each species by the total area to find fraction
species$'Fraction of Betula'<-species$`Betula Area`/species$`B+S`
species$'Fraction of Salix'<-species$`Salix Area`/species$`B+S`

#condensing that into one nice little table
plot_fraction<-species[,c(1,2,9,10)]

#or if you want to see based on overall site, not plot:
betula<-aggregate(species$`Fraction of Betula`,by=list(species$Site), FUN="mean")
colnames(betula)<-c("Site", "Fraction of Betula")
salix<-aggregate(species$`Fraction of Salix`,by=list(species$Site), FUN="mean")
colnames(salix)<-c("Site", "Fraction of Salix")
site_fraction<-full_join(betula,salix)