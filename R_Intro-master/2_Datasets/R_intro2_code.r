###########################################################################
#########This script analyzes thaw depth data from    #####################
#########the summer of 2016 in Cherskiy Russia        #####################
#########The data consists of thaw depth measured     #####################
#########below a transect or under sap flux trees     #####################
###########################################################################

#1. read in data

	#A. read in necessary packages for this R session
	#note run the commented out install.packages if you haven't installed them
	#install.packages(c("lubridate"))
		library(lubridate)	
	
	#B. set the working directory
		setwd("c:\\Users\\hkropp\\GitHub\\R_Intro\\2_Datasets")
	
	#C. read in thaw depth csv
		datTD <- read.csv("thaw_depth.csv")
	
	#D. check data
		head(datTD)
	
#2. Convert dates to day of year

	#A. specify date format
		dateTD <- as.Date(datTD$Date, "%m/%d/%Y") 	
	
	#B. extract the day of year out of the date and add to datTD
		datTD$doyTD <- yday(dateTD)
	
	#C. extract the year out of the date and add to datTD
		datTD$yearTD <- year(dateTD)
		
		####################################################
		############ lubridate       #######################
		####################################################
		#as.Date(Vector of dates or date/time, date format)
			#see here for table of date format symbols:
			# https://www.r-bloggers.com/date-formats-in-r/
		
		#yday(as.Date output) gives a vector of day of year
		
		#year(as.Date output) gives a vecotr of year
	
#3. Get the average thaw depth by site

	#A. First way to get the average
		
		ave1 <- tapply(datTD$TD, datTD$Site, FUN="mean" )
		
		####################################################
		############ tapply()        #######################
		####################################################		
		#tapply(vector, Index (can be a list of multiple vectors or a single vector of factors), FUN= "function")
		####################################################				
		
		#look at results
		ave1
		#let's get some information about what this output is
		str(ave1)
		
		#you'll see the name of the factor for each mean is linked as an object called dimnames
		#and the means are actually stored in ave 1 as vector
		dimnames(ave1)
		
		#pull out the DAV mean
		ave1[1]
		
		#you'll also note there is an inconsistancy in the name for the Davydov forest
		
#4. Fix the site name for Davydov
	
	#A. using the ifelse function we can apply a statement to the entire vector of names
		#Note: it will also be easier if we give the stand names a numerical id rather than
		#treat them as characters. 

		
		datTD$standID <- ifelse(datTD$Site == "DAV" | datTD$Site == "DAVY", 1,
							ifelse(datTD$Site == "LDF2", 2, NA))
		
		####################################################
		############ ifelse()        #######################
		####################################################
		#ifelse(logical statement, output if true, output if false)
		#Note: we just used logical statements:
		# here the == denotes is equal to
		# | means or
		####################################################		
		
		
		
	#B. setup a flag to make sure we don't have any names that don't fit in 
	    #our ifelse statement that statement

			if( length( which( is.na(datTD$standID) ) ) > 0){
				print("ERROR unnamed site")
				}else{
						print("No unnamed sites")
					}
		
		#there's a lot goinh on here
		#we might be concerned that our ifelse statement might work but it could
		#spit out NAs when it shouldn't, and we might not notice without scrolling 
		#through the data.
		
		#we just used the other version of if. This only evaluates one element at a time
		# we would need a for loop (we'll cover these in the future) to iterate through
		# the if statement above if we wanted to use it to make our standID
		
		
		####################################################
		############ IF and IF/ELSE  #######################
		####################################################
		
		#here is how this if statement works
		#if(logical statement){
			#does whatever is in these brackets if true
		#}
		
		#you can also add an else statement if you want
		
		#if(logical statement){
		# does whatever is in these brackets if true
		#}else{
				#does whatever is in these brackets if true
			#}

		####################################################
		############ length          #######################
		####################################################	
		
		#length(vector)
		
		#this will return a single number, the length of your vector
		
		####################################################
		############ which           #######################
		####################################################			
			
		#which(logical statement)
		
		#which will return the row numbers of data that meets a logical statement
		
		####################################################
		############ is.na           #######################
		####################################################		
		
		#is.na(data element)
		
		#returns a true or false if there is an NA value
		####################################################
		
#5. Get the average for the stand and day of year 			
	
		#A.  take the average across stand and day of year
			TDave<-aggregate(datTD$TD, by=list(datTD$doyTD, datTD$standID), FUN="mean")
			TDave
		#B. rename the columns to be more informative
			colnames(TDave)<-c("doy","standID", "TD")
		
		####################################################
		############ aggregate       #######################
		####################################################
		
		#aggregate(data.vector, by=list(one or more numerical index or factors), FUN="function")
		
		####################################################
		
#6. Make a plot of the average thaw depth over time	with each stand seperated	
	

		#A. set up the plot
		
			#start by plotting high density
			plot(TDave$doy[TDave$standID==1],TDave$TD[TDave$standID==1], 
				ylim=c(min(TDave$TD)-5,max(TDave$TD)+5),
				xlim=c(min(TDave$doy)-2,max(TDave$doy)+2), type="b",
				xlab="Day of Year", ylab="Thaw depth (cm)", pch=19, col="forestgreen" )
			
		#B.#add points for low density
			points(TDave$doy[TDave$standID==2],TDave$TD[TDave$standID==2],
			 pch=19, col="deepskyblue3",type="b" )
		
		#C. add a legend
		
			legend(min(TDave$doy),max(TDave$TD),
					c("high density", "low density"), 
					col=c("forestgreen", "deepskyblue3"), pch= 19, bty="n",
					cex=1.25)
		
		####################################################
		############ plot            #######################
		####################################################		
		
		#plot(x.coordinates,y.coordinates, xlim=c(lower value, upper value),
		#	ylim=c(lower value, upper value), xlab="label for axis",
		#	ylab="label for axis", pch=point.shape.number,
		#	col=" colorname", type="b" for both "p" for point "l" for line)
		
		####################################################
		############ points          #######################
		####################################################	

		#adds points to existing plot
		#points(x.coordinates,y.coordinates,pch=point.shape.number,
		#	col=" colorname", type="b" for both "p" for point "l" for line)
		
		####################################################
		############ legend          #######################
		####################################################	
		
		#adds a legend to a plot
		#legend(upper.left.x.coordinate, upper.left.y.coordinate,
		#		c(vector of labels), col=c(vector of colors for point or line) 
		#		OR fill=c(vector of colors for box fill), pch or lty arguments,
		#		bty="n" this gets rid of the ugly box, cex=+1 for bigger legend)
		####################################################
		
#7. Activities to complete on your own:

	#A. calculate the standard deviation sd() for each stand and day of year
	
	#B. add arrows to the plot using arrows() with the code argument set to zero
	
	#C. Make a plot that looks at the thaw depth for each tree in the Davydov stand
	
