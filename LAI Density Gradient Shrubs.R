#SC 6/19/19
#script to calculate LAI for siberia density gradient shrubs

#the first part of this script is modified from another script 
#i made called "Average SLA for Betula and Salix"
#i have included it here to have all the calculations
#for the SLA values to be used in one place

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

data<-read.csv("Betula_Salix data from SLA_sfarmer and SLA_2017.csv")

#split by species
bdata<-data[1:25,]
sdata<-data[26:50,]

#first, finding low density 
#as the high density plots will vary based on sun/shade value, those come next
bsla<-aggregate(bdata$SLA..m2.g., by=list(bdata$Site), FUN="mean")

#naming this value in case sla value is ever changed/recalculated
#so won't have to go through the whole code changing it
b.l.sla<-bsla[2,2]

#same for salix
ssla<-aggregate(sdata$SLA..m2.g., by=list(sdata$Site), FUN="mean")
s.l.sla<-ssla[2,2]

#now finding high density values based on sun vs shade
#NOTE: as only high density had data for sun vs shade, i will not put an "h" in front
bhigh<-bdata[1:12,]
shigh<-sdata[1:12,]

bsunsla<-aggregate(bhigh$SLA..m2.g., by=list(bhigh$Sun.Shade), FUN="mean")
b.sun.sla<-bsunsla[2,2]
b.shade.sla<-bsunsla[1,2]

ssunsla<-aggregate(shigh$SLA..m2.g., by=list(shigh$Sun.Shade), FUN="mean")
s.sun.sla<-ssunsla[2,2]
s.shade.sla<-ssunsla[1,2]

#Summary:
#Betula High Density Sun= 0.01409087
#Betula High Density Shade= 0.01939790
#Salix High Density Sun= 0.01409602
#Salix High Density Shade= 0.01499999
#Betula Low Density, SLA= 0.0102796925384615
#Salix Low Density, SLA= 0.0116642552307692

#with SLA values calculated, now we can begin working with the shrub data:

shrub_data<-read.csv("2012 - 2017 Density Gradient Shrubs.csv")[,c(1:5,8)]

#add a column for density
#using "L" for low density and "H" for high and medium density
shrub_data$density <- ifelse(substr(shrub_data$Site,1,1) == "L",
                         "L","H")

#adding another column for species (just with first letter)
shrub_data$species <- ifelse(substr(shrub_data$Species,1,1) == "B",
                             "B","S")

#combining the two to differentiate species and density
shrub_data$species.density<-paste(shrub_data$density,shrub_data$species)

colnames(shrub_data)<-c("Site", "Plot", "Plot Area", "Species", "B.D.", 
                        "New Growth", "density", "species", "species.density")

#using these, calculating the leaf area with the appropriate SLA
shrub_data$leaf.area<-ifelse(substr(shrub_data$species.density,1,3)=="H B", shrub_data$`New Growth`*b.sun.sla,
                             ifelse(substr(shrub_data$species.density,1,3)== "L B", 
                                    shrub_data$`New Growth`*b.l.sla,
                                    ifelse(substr(shrub_data$species.density,1,3)=="H S", 
                                           shrub_data$`New Growth`*s.sun.sla, 
                                           shrub_data$`New Growth`*s.l.sla)))

#find the sum of the leaf area in each site/plot
site_lai<-aggregate(shrub_data$leaf.area, 
                    by=list(shrub_data$Site, shrub_data$Plot,
                            shrub_data$`Plot Area`), FUN=sum)
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
barplot(data, xlab="Site", ylab="Average LAI", las=2,
        main="Average LAI of Density Gradient Trees and Shrubs",
        col=c("palegreen4", "chocolate4"),legend(locator(1),legend=rownames(data),
                                                 fill=c("palegreen4", "chocolate4")),beside=TRUE)


#plots for each density
#high density sites
trees<-read.csv("Average LAI for Density Gradient Trees") [,2:3]
colnames(trees)<-c("Site", "Average Tree LAI")

shrubs<-read.csv("Average LAI for Density Gradient Shrubs") [,2:3]
colnames(shrubs)<-c("Site", "Average Shrub LAI")

raw_data<-full_join(trees, shrubs)[c(1:3,5:15,17:25),]

high_data<-t(raw_data[1:10,2:3])
colnames(high_data)<-raw_data$Site[1:10]
barplot(high_data, xlab="Site", ylab="Average LAI", las=2,
        main="Average LAI of High Density Gradient Trees and Shrubs",
        col=c("palegreen4", "chocolate4"),legend(locator(1),legend=rownames(high_data),
                                                fill=c("palegreen4", "chocolate4")),beside=TRUE)

#medium density sites
med_data<-t(raw_data[18:23,2:3])
colnames(med_data)<-raw_data$Site[18:23]
barplot(med_data, xlab="Site", ylab="Average LAI", las=2,
        main="Average LAI of Medium Density Gradient Trees and Shrubs",
        col=c("palegreen4", "chocolate4"),legend(locator(1),legend=rownames(med_data),
                                                 fill=c("palegreen4", "chocolate4")),beside=TRUE)

#low density sites
low_data<-t(raw_data[11:17,2:3])
colnames(low_data)<-raw_data$Site[11:17]
barplot(low_data, xlab="Site", ylab="Average LAI", las=2,
        main="Average LAI of Low Density Gradient Trees and Shrubs",
        col=c("palegreen4", "chocolate4"),legend(locator(1),legend=rownames(low_data),
                                                 fill=c("palegreen4", "chocolate4")),beside=TRUE)


#comparing % betula vs salix in each shrub site
#finding the leaf area of each species
stand_area<-aggregate(shrub_data$leaf.area, 
                       by=list(shrub_data$Site, shrub_data$Plot,
                               shrub_data$`Plot Area`, shrub_data$Species), FUN=sum)
colnames(stand_area1)<-c("Site", "Plot", "Plot Area", "Species", "Total Leaf Area")

stand_area$LAI<-stand_area$`Total Leaf Area`/stand_area$`Plot Area`

#split into betula/salix
bstand_area<-stand_area[1:72,]
colnames(bstand_area)<-c("Site", "Plot", "Plot Area", "Species", "Area")
sstand_area<-stand_area[73:131,]
colnames(sstand_area)<-c("Site", "Plot", "Plot Area", "Species", "Area")

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

#condensing into one data frame
plot_fraction<-species[,c(1,2,9,10)]

#or if you want to see based on overall site, not plot:
betula<-aggregate(species$`Fraction of Betula`,by=list(species$Site), FUN="mean")
colnames(betula)<-c("Site", "Fraction of Betula")
salix<-aggregate(species$`Fraction of Salix`,by=list(species$Site), FUN="mean")
colnames(salix)<-c("Site", "Fraction of Salix")
site_fraction<-full_join(betula,salix)


#making plots for fraction within each site and plot
#first: within each site
site<-t(site_fraction[,2:3])
colnames(site)<-site_fraction$Site
barplot(site, xlab="Site", ylab="Average LAI", las=2,
        main="Fraction of Shrub Species in Each Site",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(high_plot),
                                               fill=c("darkturquoise", "coral")),beside=TRUE)

#the above has the bars next to each other. i'm going to make another to view the fraction
#as a section of each bar. up to preference!

barplot(site, xlab="Site", ylab="Average LAI", las=2,
        main="Fraction of Shrub Species in Each Site",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(site),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)

#those graphs are very cramped. splitting into high/medium/low density
#im going to make them all for now the second way i did above.
#to change, just change beside from FALSE to TRUE!
#high density sites
high_site<-t(site_fraction[1:10,2:3])
colnames(high_site)<-site_fraction$Site[1:10]
barplot(high_site, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in High Density Sites",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(high_site),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)

#medium density sites
med_site<-t(site_fraction[18:24,2:3])
colnames(med_site)<-site_fraction$Site[18:24]
barplot(med_site, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in Medium Density Sites",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(med_site),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)

#low density sites
low_site<-t(site_fraction[11:17,2:3])
colnames(low_site)<-site_fraction$Site[11:17]
barplot(low_site, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in Low Density Sites",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(low_site),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)


#now plots for fraction within each plot (by density)
plot_fraction<-arrange(plot_fraction, plot_fraction$Site)
plot_fraction$Plot<-paste(plot_fraction$Site, plot_fraction$Plot)
high_plot<-t(plot_fraction[1:30,3:4])
colnames(high_plot)<-plot_fraction$Plot[1:30]
barplot(high_plot, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in High Density Plots",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(high_plot),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)

med_plot<-t(plot_fraction[52:73,3:4])
colnames(med_plot)<-plot_fraction$Plot[52:73]
barplot(med_plot, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in Medium Density Plots",
        col=c("darkturquoise", "coral"),legend(locator(1),legend=rownames(med_plot),
                                               fill=c("darkturquoise", "coral")),beside=FALSE)

low_plot<-t(plot_fraction[31:51,3:4])
colnames(low_plot)<-plot_fraction$Plot[31:51]
barplot(low_plot, xlab="Site", ylab="Fraction of Species", las=2,
        main="Fraction of Shrub Species in Low Density Plots",
        col=c("darkturquoise", "coral"),beside=FALSE, legend(locator(1),legend=rownames(low_plot),
                                                             fill=c("darkturquoise","coral")))
