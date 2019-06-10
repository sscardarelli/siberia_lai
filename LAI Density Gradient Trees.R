# SC 6/10/19
# script to calculate lai for siberia density gradient sites

setwed("C:/Users/scardarelli/Documents/research/siberia_lai")

#read in data only to row 5012 because plot value is incorrect
tree_data<-read.csv("2010 - 2017 Density Gradient Trees 3_13_19.csv",
                    nrows=5011)[,c(2:6,12)]

#things I've done so far:
f_bio<-tree_data$Foliage.biomass..g.dry.wt.

hbr_1<-tree_data[1:55,1:2]

hbr_1_fb<-tree_data[1:55,c(1:2,6)]
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

hdf1_1_fb<-tree_data[944:991,c(1:2,6)]
sum(hdf1_1_fb$Foliage.biomass..g.dry.wt.)
#should equal 21251.68, just like foliar_biomass!
