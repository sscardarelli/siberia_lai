#SC 6/19/19
#script to calculate LAI for siberia density gradient shrubs

setwd("C:/Users/scardarelli/Documents/research/siberia_lai")

library(dplyr)

shrub_data<-read.csv("2012 - 2017 Density Gradient Shrubs.csv") [,c(1:5,8)]

#add a column for density
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
