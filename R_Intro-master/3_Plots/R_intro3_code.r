###########################################################################
#########This script contains code and examples       #####################
#########for making scientific quality figures.       #####################
###########################################################################


#read in example dataset
	datRH <- read.csv("c:\\Users\\hkropp\\Documents\\GitHub\\R_Intro\\3_Plots\\canopyRH.csv")
	datRH$Time <- datRH$doy + (datRH$hour/24)

#######################################################
######## Make a multipanel figure #####################
#######################################################
#this code makes a multipanel figure in a jpeg
#that will have manuscript quality label sizes.
#it uses the example dataset to generate the figure
#but it can be changed to any type of data.

#1. specify the directory where you will save your plots
	plotDir <- "c:\\Users\\hkropp\\Pictures\\examplePlots"

#2. specify the number of panels you will need in your plot
	pN <- 4

#3. specify how many columns of panes you want
	pC <- 2

#4. specify width and height of the panels
	#these numbers will depend on how big you make your jpeg
	#if they are too big of a size for the jpeg, you will get
	#a figure region too big
	wd <- 35
	hd <- 35

#5. Declare the boundries of each plot limits. This way
    #you can readily alter the plot limits without having
	#to change anything by hand

	#I am going to make a panel of relative humidity and 
	#temperature across each hourfor each of my sites. 
	#Each site has a different time period.
	#I will stack the graphs so that each column in the
	#panel is a site since it shares the same values on 
	#the x axis.
	#x axis limits
	
	#site 1
	xl1	<- 181
	xh1 <- 183
	#site 2
	xl2 <- 183
	xh2 <- 185
	#y axis limits
	#temperature
	ylT <- 0
	yhT <- 25
	#relative humidity
	ylR <- 0
	yhR <- 1
	
	#axis label sequences
	ysT <- seq(0,20, by=5)
	ysR <- seq(0,.9, by=.1)
	xs1 <- seq(181,182.5,by=.5)
	xs2 <- seq(183,185,by=.5)
	
	
	#character expansions
	#cex for plot symbols
	cP <- 8
	#for y axis
	cAy <- 5
	#for axis labesl
	clabel <- 6
	#for x axis
	cAx <- 4
	
	#colors for plot symbols
	col1 <- "royalblue3"
	col2 <- "tomato3"
	
	#line thickness
	lw <- 3
	
	#panel letter size and location
	#cex for letter label
	cl <- 8
	#offset from panels
	l.offs <- .1
	l.offT <- 1
	l.offR <-.05
	
	
#6. Start generating the plot
	#this command will start plotting to file, and anything after it will go to your file.
	#It can only be ended with the dev.off() function.
	#the plot will be generated into the directory plotDir indicated above
	jpeg(paste0(plotDir, "\\multipanel.jpg"), width=2500, height=2500, units="px", quality=100)	
		####################################################
		############ jpeg            #######################
		####################################################
		#jpeg(file.name and path, width=width of file, height=height of file,
		#     units="px" indicated pixel units, quality=100 (lower numbers will produce lower quality))

# 7. specify the panel layout
	#the layout is based on the number of panels declared above  (pn),
	#the number of columns specified above (pc), and the width and height
	#of each panel (wd, hd respectively) that was specified above
	 layout(matrix(seq(1,pN), byrow=FALSE, ncol=pC), width=rep(lcm(wd),pN), height=rep(lcm(hd),pN))
		####################################################
		############ layout          #######################
		####################################################
		#layout(matrix that is structured to the layout of the panels that contains the plot
		#order for each panel, width=widths for each panel, height=heights for each panel)
# 8. start making the plot for panel 1: high density Temp
	#8.A. Specify that there should be no margins to the plot
	par(mai=c(0,0,0,0))
	
	#8.B. Make an empty plot with the proper limits above
	# all points, labels, and axes will be added in seperately.
	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(ylT,yhT), axes=FALSE, xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
	#8. C. add the points that we want to plot
	points(datRH$Time[datRH$site=="hd"],datRH$TempC.VP4[datRH$site=="hd"], type="b",
			col=col1,  lwd=lw, cex=cP, pch=19)
			
	#8. D. make axis for y axis	
	axis(2, ysT, cex.axis=cAy, las=2, lwd.ticks=3)
	
	#8. E. add a letter to label the panel
	text(xl1+l.offs,yhT-l.offT, "A", cex=cl)
	
	#8. F. label the y axis
	mtext("Temperature (C)", side=2, cex=clabel, line=15)
	
	#8. G. make a box around the plot
	box(which="plot")
	
	#8. H. label the site
	mtext("High density", side=3, cex=clabel, line=5)
	
# 9. start making the plot for panel 2: high density RH
	#9.A. Specify that there should be no margins to the plot
	par(mai=c(0,0,0,0))	
	
	#9.B. Make an empty plot with the proper limits above
	# all points, labels, and axes will be added in seperately.
	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl1,xh1), ylim=c(ylR,yhR), axes=FALSE, xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
	#9. C. add the points that we want to plot
	points(datRH$Time[datRH$site=="hd"],datRH$RH.VP4[datRH$site=="hd"], type="b",
			col=col1,  lwd=lw, cex=cP, pch=19)
			
	#9. D. make axis for y axis	
	axis(2, ysR, cex.axis=cAy, las=2, lwd.ticks=3)	
	
	
	#9. E. add a letter to label the panel
	text(xl1+l.offs,yhR-l.offR, "B", cex=cl)
	
	#9. F. label the y axis
	mtext("Relative humidity (-)", side=2, cex=clabel, line=15)
	
	#9. G. make a box around the plot
	box(which="plot")
	
	#9. H. make labels for the xaxis
	#x axis labels don't get offset from the axis as well in jpeg file set up.
	#they will be set up a little differently
	#first add axis ticks
	axis(1, xs1, rep(" ", length(xs1)),  lwd.ticks=3)
	mtext(xs1, at=xs1, side=1, line=4, cex=cAx)
	
	#9. I. add an outer label for both bottom panels since they have the same measure
	mtext("Day of year", side=1, outer=TRUE, line=-7, cex=clabel)
	
# 10. start making the plot for panel 3: low density Temp
	#10.A. Specify that there should be no margins to the plot
	par(mai=c(0,0,0,0))
	
	#10.B. Make an empty plot with the proper limits above
	# all points, labels, and axes will be added in seperately.
	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(ylT,yhT), axes=FALSE, xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
	#10. C. add the points that we want to plot
	points(datRH$Time[datRH$site=="ld"],datRH$TempC.VP4[datRH$site=="ld"], type="b",
			col=col2,  lwd=lw, cex=cP, pch=19)
			
	
	#10. D. add a letter to label the panel
	text(xl2+l.offs,yhT-l.offT, "C", cex=cl)	
	
	#10. E. make a box around the plot
	box(which="plot")
	
	#10. F. label the site
	mtext("Low density", side=3, cex=clabel, line=5)
	
# 11. start making the plot for panel 2: high density RH
	#11.A. Specify that there should be no margins to the plot
	par(mai=c(0,0,0,0))	
	
	#11.B. Make an empty plot with the proper limits above
	# all points, labels, and axes will be added in seperately.
	
	plot(c(0,1),c(0,1), type="n", xlim=c(xl2,xh2), ylim=c(ylR,yhR), axes=FALSE, xlab=" ",
		ylab=" ", xaxs="i", yaxs="i")
		
	#11. C. add the points that we want to plot
	points(datRH$Time[datRH$site=="ld"],datRH$RH.VP4[datRH$site=="ld"], type="b",
			col=col2, lwd=lw, cex=cP, pch=19)
			
		
	#11. D. add a letter to label the panel
	text(xl2+l.offs,yhR-l.offR, "D", cex=cl)
	
	#11. E. make a box around the plot
	box(which="plot")
	
	#11. F. make labels for the xaxis
	#x axis labels don't get offset from the axis as well in jpeg file set up.
	#they will be set up a little differently
	#first add axis ticks
	axis(1, xs2, rep(" ", length(xs2)),  lwd.ticks=3)
	mtext(xs2, at=xs2, side=1, line=4, cex=cAx)
	
#12. turn off plot
dev.off()


######End multipanel figure############################
