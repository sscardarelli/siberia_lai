# SC 6/10/19
# script to calculate lai for siberia density gradient sites

setwed("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

#read in data only to row 5011 because plot value is incorrect
tree_data<-read.csv("2010 - 2017 Density Gradient Trees 3_13_19.csv",
                    nrows=5011)[,c(2:7,12)]

#things I've done so far:
f_bio<-tree_data$Foliage.biomass..g.dry.wt.

hbr_1<-tree_data[1:55,1:2]

hbr_1_fb<-tree_data[1:55,c(1:2,7)]
sum(hbr_1_fb$Foliage.biomass..g.dry.wt.)

#so from here I got the sum of just one site of one plot. this would take a loooonnnggg time
#to do one at a time for each so. gotta figure out a quicker way.

#ok think i figured it out!!!
foliar_biomass<-aggregate(tree_data$Foliage.biomass..g.dry.wt., 
                          by=list(tree_data$Site, tree_data$Plot), FUN=sum)

#and we can double check if they're correct by checking a few of the smaller ones
#like how i made earlier. for example:

sum(hbr_1_fb$Foliage.biomass..g.dry.wt.)
#should equal 14621.75
#first value on the foliar_biomass is also 14621.75!

hdf1_1_fb<-tree_data[944:991,c(1:2,7)]
sum(hdf1_1_fb$Foliage.biomass..g.dry.wt.)
#should equal 21251.68, just like foliar_biomass!

#rename columns
colnames(foliar_biomass)<-c("Site","Plot","Sum of Foliage Biomass")
foliar_biomass

#reorganize by Site name, not Plot
fol_bio<-arrange(foliar_biomass, foliar_biomass$Site)

#dplyr
tree_data %>%
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
high_dens$leaf_area<-high_dens$total_foliar*1.1289
low_dens$leaf_area<-low_dens$total_foliar*0.8469

#all my tables are messy/full of columns so I'm just going to condense quickly
#you may not need to do this
low_density<-low_dens[c(1:3,6,8,9,11)]
high_density<-high_dens[c(1:3,6,8,9,11)]

#renaming columns
colnames(high_density)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                          "Density", "Foliar Biomass (g)", "Leaf Area (m^2)")
colnames(low_density)<-c("Site", "Plot", "Sample Area", "Diameter (cm)", 
                         "Density", "Foliar Biomass (g)", "Leaf Area (m^2)")

#joining tables now as high/low are done being treated differently
#thought might make it easier on me moving forward
lai_data<-full_join(high_density,low_density)

#now calculate LAI! add a column to the table for this
lai_data$LAI<-lai_data$`Leaf Area (m^2)`/lai_data$`Sample Area`

#condense by site and plot
tree_lai_data<-aggregate(lai_data$LAI, by=list(lai_data$Site, lai_data$Plot), FUN=sum)
colnames(tree_lai_data)<-c("Site", "Plot", "LAI")
tree_lai_data<-arrange(tree_lai_data, tree_lai_data$Site)

#practice making it with dplyr
tree_lai_data1<-lai_data %>%
  group_by(Site, Plot) %>%
  summarise(LAI = sum(LAI)) %>%
  arrange(Site)
