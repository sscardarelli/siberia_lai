# SC 6/10/19
# script to calculate lai for siberia density gradient sites

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

#read in data only to row 5011 because plot value is incorrect
tree_data<-read.csv("2010 - 2017 Density Gradient Trees 3_13_19.csv",
                    nrows=5011)[,c(2:7,12)]

#creating another column for density: "H" or "L"
#we need to use different allometry for low density and high & medium density sites
#medium density sites will be treated like high density
#this column will allow us to differentiate between the two
tree_data$dens <- ifelse(substr(tree_data$Site,1,1) == "L",
                          "L","H")

#recalculating foliar biomass with new allometry
by_dens <- arrange(tree_data,tree_data$dens)

#i split into high density and low density charts
high_dens<-by_dens[1:4217,]
low_dens<-by_dens[4218:5011,]

#and calculate new biomass on both, adding to a new column
high_dens$total_foliar<-7.57*high_dens$Diam..cm.^1.73
low_dens$total_foliar<-150.5*low_dens$Diam..cm

#calculate leaf area using SLA for high and low density
#the SLA i was given was in cm^2 so i converted to m^2 for this
high_dens$leaf_area<-high_dens$total_foliar*(112.89/10000)
low_dens$leaf_area<-low_dens$total_foliar*(84.69/10000)

#all my tables are messy/full of columns so I'm just going to condense quickly
low_dens<-low_dens[c(1:3,6,9,10)]
high_dens<-high_dens[c(1:3,6,9,10)]

#renaming columns
colnames(high_dens)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                          "Foliar Biomass (g)", "Leaf Area (m^2)")
colnames(low_dens)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                          "Foliar Biomass (g)", "Leaf Area (m^2)")

#joining tables now as high/low are done being treated differently
leaf_area<-full_join(high_dens,low_dens)

#6/13/19 edit: adding up leaf area per site/plot
tlai<-aggregate(leaf_area$`Leaf Area (m^2)`, 
                           by=list(leaf_area$Site, leaf_area$Plot, leaf_area$`Sample Area`), FUN=sum)
colnames(tlai)<-c("Site", "Plot", "Plot Area", "Total Leaf Area")
tlai<-arrange(tlai, tlai$Site)

#now calculate LAI and add a column to the table for this
tlai$LAI<-tlai$`Total Leaf Area`/tlai$`Plot Area`

#average of all three plots in each site
average<-aggregate(tlai$LAI, by=list(tlai$Site), FUN=mean)
colnames(average)<-c("Site", "Average LAI")

#take standard deviation
sd<-aggregate(tlai$LAI, by=list(tlai$Site), FUN=sd)
colnames(sd)<-c("Site","Standard Deviation")

#join the two tables into one data sheet
lai_trees<-full_join(average,sd)

#saving as csv
write.csv(lai_trees, "Average LAI for Density Gradient Trees")
