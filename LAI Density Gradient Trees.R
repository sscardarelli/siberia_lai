# SC 6/10/19
# script to calculate lai for siberia density gradient sites

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

#read in data only to row 5011 because plot value is incorrect
tree_data<-read.csv("2010 - 2017 Density Gradient Trees 3_13_19.csv",
                    nrows=5011)[,c(2:4,7,12)]

#creating another column for density: "H" or "L"
#we need to use different allometry for low density and high & medium density sites
#medium density sites will be treated like high density
#this column will allow us to differentiate between the two
tree_data$dens <- ifelse(substr(tree_data$Site,1,1) == "L",
                          "L","H")

#recalculating foliar biomass with the new allometry
tree_data$foliar.biomass<-ifelse(substr(tree_data$dens,1,1)=="L", 
                                 150.5*tree_data$Diam..cm., 7.57*tree_data$Diam..cm.^1.73)

#calculate leaf area
#SLA was given to me in centimeters, so i converted it to meters:
tree_data$leaf.area<-ifelse(substr(tree_data$dens,1,1)=="L",
                            tree_data$foliar.biomass*(84.69/10000),
                            tree_data$foliar.biomass*(112.89/10000))

#add up leaf area per site/plot
tlai<-aggregate(tree_data$leaf.area, 
                           by=list(tree_data$Site, tree_data$Plot, tree_data$Area.sampled..m2.), FUN=sum)
colnames(tlai)<-c("Site", "Plot", "Plot Area", "Total Leaf Area")
tlai<-arrange(tlai, tlai$Site)

#now calculate LAI and add a column to the table for this
tlai$LAI<-tlai$`Total Leaf Area`/tlai$`Plot Area`

#average of all three plots in each site
average<-aggregate(tlai$LAI, by=list(tlai$Site), FUN="mean")
colnames(average)<-c("Site", "Average LAI")

#take standard deviation
sd<-aggregate(tlai$LAI, by=list(tlai$Site), FUN=sd)
colnames(sd)<-c("Site","Standard Deviation")

#join the two tables into one data sheet
lai_trees<-full_join(average,sd)

#saving as csv
write.csv(lai_trees, "Average LAI for Density Gradient Trees")
