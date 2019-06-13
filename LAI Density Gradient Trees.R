# SC 6/10/19
# script to calculate lai for siberia density gradient sites

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

#read in data only to row 5011 because plot value is incorrect
tree_data<-read.csv("2010 - 2017 Density Gradient Trees 3_13_19.csv",
                    nrows=5011)[,c(2:7,12)]

#calculate the total foliar biomass
foliar_biomass<-aggregate(tree_data$Foliage.biomass..g.dry.wt., 
                          by=list(tree_data$Site, tree_data$Plot), FUN=sum)

#rename columns
colnames(foliar_biomass)<-c("Site","Plot","Sum of Foliage Biomass")
foliar_biomass

#reorganize by Site name, not Plot
fol_bio<-arrange(foliar_biomass, foliar_biomass$Site)

#trying to generate same table from dplyr
foliar_biomass1<-tree_data %>%
  group_by(Site, Plot) %>%
  summarise(total_biomass = sum(Foliage.biomass..g.dry.wt.)) %>%
  arrange(Site)

#creating another column for density: "H" or "L"
#we need to use different allometry for low density and high & medium density sites
#this column will allow us to differentiate between the two
tree_data$dens <- ifelse(substr(tree_data$Site,1,1) == "L",
                          "L","H")

#recalculating foliar biomass with new allometry
by_dens <- arrange(tree_data,tree_data$dens)

#for now, split into high density and low density charts
#(until I can find a better way)
high_dens<-by_dens[1:4217,]
low_dens<-by_dens[4218:5011,]

high_diameter<-high_dens$Diam..cm.
low_diameter<-low_dens$Diam..cm.

#and calculate new biomass on both, adding to a new column
high_dens$total_foliar<-7.57*high_diameter^1.73
low_dens$total_foliar<-150.5*low_diameter

#calculate leaf area using SLA for high and low density
#the SLA i was given was in cm^2 so i converted to m^2 for this
high_dens$leaf_area<-high_dens$total_foliar*(112.89/10000)
low_dens$leaf_area<-low_dens$total_foliar*(84.69/10000)

#all my tables are messy/full of columns so I'm just going to condense quickly
#you may not need to do this
low_density<-low_dens[c(1:3,6,9,10)]
high_density<-high_dens[c(1:3,6,9,10)]

#renaming columns
colnames(high_density)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                          "Foliar Biomass (g)", "Leaf Area (m^2)")
colnames(low_density)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                          "Foliar Biomass (g)", "Leaf Area (m^2)")

#joining tables now as high/low are done being treated differently
#thought might make it easier on me moving forward
lai_data<-full_join(high_density,low_density)

#6/13/19 edit: adding up leaf area per site/plot
leaf_area<-aggregate(lai_data$`Leaf Area (m^2)`, 
                           by=list(lai_data$Site, lai_data$Plot, lai_data$`Sample Area`), FUN=sum)
colnames(leaf_area)<-c("Site", "Plot", "Plot Area", "Total Leaf Area")
leaf_area<-arrange(leaf_area, leaf_area$Site)

#same table with dplyr
colnames(lai_data)<-c("Site", "Plot", "Sample_Area", "Diameter", "Biomass", "Leaf_Area")
leaf_area1<-lai_data %>%
  group_by(Site, Plot, Sample_Area) %>%
  summarise(Total_Area=sum(Leaf_Area))

#now calculate LAI and add a column to the table for this
leaf_area$LAI<-leaf_area$`Total Leaf Area`/leaf_area$`Plot Area`

#average of all three plots in each site
average<-aggregate(leaf_area$LAI, by=list(leaf_area$Site), FUN=mean)
colnames(average)<-c("Site", "Average LAI")

#with dplyr
average1<- leaf_area %>%
  group_by(Site) %>%
  summarise(Average_LAI=mean(LAI))

#take standard deviation
sd<-aggregate(leaf_area$LAI, by=list(leaf_area$Site), FUN=sd)
colnames(sd)<-c("Site","Standard Deviation")

#do this also with dplyr (for practice)
sd1<-leaf_area %>%
  group_by(Site) %>%
  summarise(Standard_Deviation=sd(LAI))

#join the two tables into one data sheet
lai_trees<-full_join(average,sd)

#saving as csv
write.csv(lai_trees, "Average LAI for Density Gradient Trees")
