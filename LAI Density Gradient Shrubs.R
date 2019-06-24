#SC 6/19/19
#script to calculate LAI for siberia density gradient shrubs

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

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

#now that they're separated we can do the appropriate calculations to them for leaf area
#using SLA found by averaging data on SLA_sfarmer and SLA_2017
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

#aggregate based on site/plot
shrub_lai<-aggregate(leafarea$leaf_area, 
                     by=list(leafarea$Site, leafarea$Plot,
                             leafarea$Area.Sampled..m2.,leafarea$Species), FUN=sum)
colnames(shrub_lai)<-c("Site", "Plot", "Plot Area", "Species","Total Leaf Area")
shrub_lai<-arrange(shrub_lai, shrub_lai$Site)

#and now calculate LAI
shrub_lai$LAI<-shrub_lai$`Total Leaf Area`/shrub_lai$`Plot Area`

#TOMORROW: do more work with this
#Find LAI and take average value across each site: no need for separate species
site_lai<-aggregate(leafarea$leaf_area, 
                    by=list(leafarea$Site, leafarea$Plot,
                            leafarea$Area.Sampled..m2.), FUN=sum)
colnames(site_lai)<-c("Site", "Plot", "Plot Area", "Total Leaf Area")
site_lai$LAI<-

#average/sd for each site
average1<-aggregate(shrub_lai$LAI, by=list(shrub_lai$Site), FUN="mean")
sd1<-aggregate(shrub_lai$LAI, by=list(shrub_lai$Site), FUN=sd)

#now average/sd for each species within the site
average<-aggregate(shrub_lai$LAI, by=list(shrub_lai$Site, shrub_lai$Species), FUN="mean")
sd<-aggregate(shrub_lai$LAI, by=list(shrub_lai$Site, shrub_lai$Species), FUN=sd)

#give column names and join tables
colnames(average)<-c("Site","Species","Average LAI")
colnames(sd)<-c("Site","Species","Standard Deviation")
lai_shrubs<-full_join(average,sd)
