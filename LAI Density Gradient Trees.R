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
#this is currently using SLA with cm/g as units cause diameter is in cm and foliar in g
high_dens$leaf_area<-high_dens$total_foliar*112.89
low_dens$leaf_area<-low_dens$total_foliar*84.69
#these are very high numbers and I've never done this before so idk if they're right

#all my tables are messy/full of columns so I'm just going to condense quickly
#you may not need to do this
low_density<-low_dens[c(1:3,6,8,9,11)]
high_density<-high_dens[c(1:3,6,8,9,11)]

#renaming columns
