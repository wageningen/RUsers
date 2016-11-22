
#########################################
# R Users Meeting 27 January 2015       #
# "Making plots in R"                   #
# Karen Kloth, Wageningen Univerisity   #
#                                       #
# Content:                              #
# 1. Boxplots                           #
#     - colours                         #
#     - titles                          #
#     - tick marks                      #
#     - saving plots                    #
# 2. Scatter plots                      #
#     - linear model                    #
#     - multiple plots in panels        #
#     - margins                         #
#     - colour by 3rd variable          #
#     - legend                          #
# 3. Histograms                         #
#     - count-density                   #
#     - Kernel density                  #
#     - colour area under curve         #
#     - overlapping curves/histograms   # 
# 4. Bar plots                          #
#     - mean values                     #
#     - x-axis labels and order         #         
#     - y axis range                    #
#     - grouping bars                   #
#     - back-to-back barplot            # 
#     - textile patterns                #
#     - error bars                      #
#     - text annotations                #
#     - interrupted y axis              # 
#     - stacked bar plot                #
# 5. Line plots                         #
#     - multiple lines                  #
#     - 2nd y-axis                      #
#########################################

rm(list=ls(all=TRUE))
setwd("D:\\Karen\\Rusers\\Makingplots2_27Jan2015")
library(reshape)
library(plotrix)

################################################

# Open data file
read.table("Developmenttime.csv", header=TRUE, sep=",",dec = ".",colClasses = "character")-> b
head(b)
ncol(b)

# Classify data
b <- within(b, {
  field <- factor(field)  
  species1_2009 <- as.numeric(species1_2009)
  species1_2010 <- as.numeric(species1_2010)
  species1_2011 <- as.numeric(species1_2011)
  species2_2009 <- as.numeric(species2_2009)
  species2_2010 <- as.numeric(species2_2010)
  species2_2011 <- as.numeric(species2_2011)
  })

################################################
## Boxplots

boxplot(b[,2:7])
boxplot(b[,c(2,5)])
# boxplots show median (notch), interquartile range (box), 
# data within 1.5x the interquartile range (whiskers), and outliers (dots)

# Titles and font size
boxplot(b[,2:7],main="Development time",ylab="Time (days)", xlab="species-year",
        cex.main=1.5,cex.lab=1.3)

### Colours
## define with a code/name (http://research.stowers-institute.org/efg/R/Color/Chart/index.htm) 
# 1 colour
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col="lightblue")
# many colours
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,cex.axis=0.5,col=c("blue","orange","red","pink","green","yellow"))
# colour by species
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=c(rep("lightblue",3),rep("blue",3)))

## or use colour palette (http://stat.ethz.ch/R-manual/R-devel/library/grDevices/html/palettes.html, http://cran.r-project.org/web/packages/RColorBrewer/index.html)
# heat.colors
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=heat.colors(6,alpha=1))
# rainbow
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=rainbow(6))
# rainbow - defined range
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=rainbow(6,start=0.9,end=0.3))

# Spacing of tick marks
# yaxp=c() or xaxp=c()
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=heat.colors(6,alpha=1),yaxp=c(0,30,3))

## Save plot: chose a file format (pdf, tiff, png, ...)
pdf("Dev.pdf", width=12, height=8)
# or in png format: png("Dev.png"), tiff("Dev.tiff")
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=heat.colors(6,alpha=1),yaxp=c(0,30,3))
dev.off()

tiff("Dev.tiff", width=800, height=600)
boxplot(b[,2:7],main="Development time",ylab="Time (days)",xlab="species-year",
        cex.main=1.5,cex.lab=1.3,col=heat.colors(6,alpha=1),yaxp=c(0,30,3))
dev.off()

# file will be saved in your working directory

################################################
## Scatter plots
plot(b$species1_2009~b$species1_2010)
# Change symbol (pch=)
# http://www.statmethods.net/advgraphs/parameters.html
plot(b$species1_2009~b$species1_2010,pch=2)
# fit a linear model
lm.species1 <- lm(b$species1_2009~b$species1_2010)
abline(lm.species1,lwd=2, col="red")

## Show multiple graphs in panels
# compare all columns
plot(b)
plot(b,upper.panel=NULL)
plot(b,lower.panel=NULL)

# Customise x and y axes
par(mfrow=c(3,2))
plot(b$species1_2009~b$species1_2010)
plot(b$species1_2009~b$species2_2009)
plot(b$species1_2009~b$species2_2010)
plot(b$species1_2010~b$species2_2009)
plot(b$species1_2010~b$species2_2010)
plot(b$species2_2009~b$species2_2010)
# Maybe you get an error message and otherwise ugly margins!

# Solve by decreasing the margin size:
# http://research.stowers-institute.org/efg/R/Graphics/Basics/mar-oma/index.htm
par("mar")
# Default: 5.1 below, 4.1 left, 4.1 above, 2.1 right
# adjust:
par(mar=c(2,4,1,0.5))
plot(b$species1_2009~b$species1_2010)
plot(b$species1_2009~b$species2_2009)
plot(b$species1_2009~b$species2_2010)
plot(b$species1_2010~b$species2_2009)
plot(b$species1_2010~b$species2_2010)
plot(b$species2_2009~b$species2_2010)

# Force plots to be square:
par(pty="s") # pty="m" for maximum plot size
plot(b$species1_2009~b$species1_2010)
plot(b$species1_2009~b$species2_2009)
plot(b$species1_2009~b$species2_2010)
plot(b$species1_2010~b$species2_2009)
plot(b$species1_2010~b$species2_2010)
plot(b$species2_2009~b$species2_2010)

#####################
# N.B. settings in par() will apply to ALL plots and ALL plot elements in your 
# R session, unless you change them

## General info about par() graphical parameters:
# http://stat.ethz.ch/R-manual/R-patched/library/graphics/html/par.html
# http://www.statmethods.net/advgraphs/parameters.html
#####################

## Colour by 3rd numerical variable "development time species2 2009"
# Divide in 2 groups (low and high values of 3rd variable)
?cut
s2.cols <- cut(b$species1_2009,2,labels=c("pink","red"))
cbind(b$species1_2009,as.character(s2.cols)) 

par(mfrow=c(1,1),mar=c(5,4,4,2))
plot(b$species1_2010~b$species1_2009,col=as.character(s2.cols),pch=16)

## Colour by 3rd nominal variable "field"
# automically assigned colours:
plot(b$species1_2009~b$species1_2010, col=as.character(b$field),pch=16)
# customise colours:
levels(b$field)
field.col <- ifelse(b$field==1,"pink",
                      ifelse(b$field==2,"red",
                             ifelse(b$field==3,"purple","gray")))
cbind(b$species2_2009,as.character(field.col)) 
plot(b$species1_2009~b$species1_2010, col=as.character(field.col), pch=16)

# Add legend
plot(b$species1_2009~b$species1_2010, col=as.character(field.col), pch=16)
legend(x=18,y=35,as.character(levels(b$field)),col=c("pink","red","purple","gray"),pch=16,
       title="Field",bty='n')

################################################
## Histograms

# of counts
hist(b$species1_2009)

p1 <-hist(b$species1_2009,col="red")
p2<- hist(b$species2_2010,col="blue")

# Combine both histograms, overlap different colour
# rgb=(red,green,blue,alpha). Use alpha (transparency)
plot(p1, col=rgb(1,0,0,1),xlim=c(0,40))
plot(p2, col=rgb(0,0,1,0.5),xlim=c(0,40),add=T)

# Change number of bars
hist(b$species1_2009,col="red")
hist(b$species1_2009,col="red",breaks=20) #total number of bars = 20
hist(b$species1_2009,col="red",breaks=seq(15,35,1)) #or define interval for each bar

# Density (total area=1)
hist(b$species1_2009,col="red",breaks=20,freq=F)

## kernel density plot
# Species 1
d1 <- density(b$species1_2009)
plot(d1,xlim=c(0,50),ylim=c(0,0.2))
polygon(d1,col="red")
# Add species 2
d2 <- density(b$species2_2010)
par(new=TRUE)
plot(d2,xlim=c(0,50),ylim=c(0,0.1))
polygon(d2,col=rgb(0,0,1,0.7))

################################################

## Barplots 

# has more options than histogram, for example:
# vertical bars
barplot(b$species1_2009)
# horizontal bars
barplot(b$species1_2009,horiz=T)

## For multiple species
# Change data layout from wide to long
head(b)
library(reshape)
b2 <-melt(b,id.vars=c("field"),measure.vars=c("species1_2009","species1_2010","species1_2011",
                                              "species2_2009","species2_2010","species2_2011"))
head(b2)
tail(b2)
colnames(b2)[2] <- "species"
colnames(b2)[3] <- "dev"
head(b2)

# Bar plot with bars representing single values
barplot(b2$dev)

## Now with bars representing mean value per species/year combination
# Make a dataframe with the mean and standard error
b2.mean <- aggregate(b2$dev,by=list(b2$species),mean,na.rm=T)
b2.mean
colnames(b2.mean) <- c("species","mean")
b2.sd <- aggregate(b2$dev,by=list(b2$species),sd,na.rm=T)
b2.sd
b2.sd$se <- b2.sd$x/sqrt(20)
colnames(b2.sd) <- c("species","sd","se")
b2.mean <- merge(b2.mean,b2.sd,by="species")

barplot(b2.mean$mean)

## Add labels x-axis
# names.arg=c()
barplot(b2.mean$mean,names.arg=c("species1_2009","species1_2010","species1_2011",
                                 "species2_2009","species2_2010","species2_2011"),cex.names=0.5)
# Or quicker:
barplot(b2.mean$mean,names.arg=levels(b2.mean$species),cex.names=0.5)
# Vertical bar labels
barplot(b2.mean$mean,names.arg=levels(b2.mean$species),las=2)
# Make shorter bar labels
levels(b2.mean$species)
levels(b2.mean$species) <- c("S1 2009","S1 2010","S1 2011","S2 2009","S2 2010","S2 2011")
barplot(b2.mean$mean,names.arg=levels(b2.mean$species),las=2)

## Change the order of the bars:
# First make a column where you assign a number to each bar in the required order 
b2.mean$barorder <- ifelse(b2.mean$species=="S1 2009",1,
                           ifelse(b2.mean$species=="S2 2009",2,
                                  ifelse(b2.mean$species=="S1 2010",3,
                                         ifelse(b2.mean$species=="S2 2010",4,
                                                ifelse(b2.mean$species=="S1 2011",5,6)))))
# Change order of dataframe
b2.mean.ord <- b2.mean[order(b2.mean$barorder),]
# Plot
barplot(b2.mean.ord$mean,names.arg=c("S1 2009","S2 2009","S1 2010","S2 2010","S1 2011","S2 2011"),las=2)

#N.B. If you use packages such as sciplot or ggplot2 you can also use this function to change bar orders:
#b2.mean$species <- factor(b2.mean$species,
#                       levels=c("S1 2009","S2 2009","S1 2010","S2 2010","S1 2011","S2 2011"),
#                       ordered=TRUE)
#and then call your plot function
#It does however not work with barplot()


## Customise y-axis
# Define range
# ylim=c() or xlim=c()
barplot(b2.mean$mean,names.arg=levels(b2.mean$species),ylim=c(0,35),las=2)
# Add axis title and plot title
# ylab, xlab, main
barplot(b2.mean$mean,names.arg=levels(b2.mean$species),ylim=c(0,35),
        ylab="Time (days)", main="Development time",cex.lab=1.2)
# Add horinzontal line
abline(h=b2.mean[1,2],lty=2)#value from your data set
abline(h=18,lty=2)# hard value
# Add vertical line
abline(v=3.7,lty=2,lwd=2)

## Group bars according to species
# Change data layout: 
# 1 column per species (for 1 stacked bar per species)
# 1 row per year (each layer in the stacked bars represents a year)

#Current layout
b2.mean
# Make 1 column with species
b2.mean$sp <- c(rep("S1",3),rep("S2",3))
# Make 1 column with year
b2.mean$yr <- c(2009,2010,2011,2009,2010,2011)
# Remove 1st,3rd, and 5th column
b2.mean2 <- b2.mean[,c(2,6,7)]
b2.mean2

# Change layout to 1 column per species
b2.mean2 <- reshape(b2.mean2,             #source (data frame)
                    idvar = "yr",       #column that identifies individual
                    v.names = "mean",  #column to be split into column sets
                    timevar = "sp",      #column with indices to the new columns to be formed
                    direction = "wide")     #from long to wide
b2.mean2
colnames(b2.mean2) <- c("yr","Species 1","Species 2")
b2.mean2

# change dataframe in matrix without first column
b2.mean2 <- data.matrix(b2.mean2[,2:3])
b2.mean2
barplot(b2.mean2,beside=T)

##############################

## Back-to-back barplot

# vertical direction
barplot(-b2.mean2[,1],beside=T,ylim=c(-30,30),yaxt="n",xaxt="n")
barplot(b2.mean2[,2],beside=T,ylim=c(-30,30),yaxt="n",names.arg=c(2009,2010,2011),col="gray25",add=T)
axis(2,ylim=c(-30,30),labels=c(30,20,10,0,10,20,30),at=c(-30,-20,-10,0,10,20,30),col="black")
mtext(2,text="Time (days)",line=2,font=2)

# horizontal direction
barplot(-b2.mean2[,1],beside=T,xlim=c(-30,30),xaxt="n",yaxt="n",horiz=T)
barplot(b2.mean2[,2],beside=T,xlim=c(-30,30),xaxt="n",names.arg=c(2009,2010,2011),las=2,col="gray25",horiz=T,add=T)
axis(1,xlim=c(-30,30),labels=c(30,20,10,0,10,20,30),at=c(-30,-20,-10,0,10,20,30),col="black")
mtext(1,text="Time (days)",line=2,font=2)


##############################

## Textile 
# define angle of dashed lines for each bar
barangle <- c(135,45,0)
# define density of dashed lines for each bar
bardensity <- c(12,30,0)
# line width
par(lwd=2)

# First barplot with background colour
barplot(b2.mean2,ylim=c(0,35),col=rep("white",3),
        yaxt='n',xaxt='n',beside=T)
# Then add dashed lines and titles with "add=T" 
barplot(b2.mean2,ylim=c(0,35),
        col="black",ylab="Time (days)", main="Development time", cex.lab=1.2,
        angle=barangle,density=bardensity,beside=T,add=T)

# Add legend
legend("topright",c("2009","2010","2011"),fill=rep("white",3),horiz=F,bty="n")
legend("topright",c("2009","2010","2011"),angle=barangle,density=bardensity,horiz=F,bty="n")

#################
# Instead of legend add year label at x-axis

# First barplot with species names
barplot(b2.mean2,ylim=c(0,35),col=rep("white",3),yaxt='n',beside=T)
# Add year labels 
barplot(b2.mean2,names.arg=rep(c("2009","2010","2011"),2),ylim=c(0,35),
        col="black",ylab="Time (days)", main="Development time", cex.lab=1.2,
        angle=barangle,density=bardensity,beside=T,add=T)

# Names on x-axis are overlapping! 
# To change it, use mgp() function
# by default mgp=c(3,1,0)
# 1st value: distance between axis label and axis,
# 2nd value: distance between tick mark labels and axis
# 3rd value: distance tick marks

# First barplot with species names at large distance from x-axis
barplot(b2.mean2,ylim=c(0,35),col=rep("white",3),yaxt='n',mgp=c(3,3,0),font=2,beside=T)
# Add year labels and axis title at normal distance
barplot(b2.mean2,names.arg=rep(c("2009","2010","2011"),2),ylim=c(0,35),
        col="black",ylab="Time (days)", main="Development time", cex.lab=1.2,
        angle=barangle,density=bardensity,mgp=c(3,1,0),beside=T,add=T)

##############################

## Add error bars
# Get x-axis position of bars:
barcenters <- barplot(b2.mean2,ylim=c(0,35),yaxt='n',xaxt='n',
                      col="black",cex.lab=1.2,
                      angle=barangle,density=bardensity,mgp=c(3,1,0),beside=T,add=T)
# Error bars without whiskes
# Remember: we calculated the standard error in b2.mean
segments(x0=barcenters,y0=b2.mean$mean-b2.mean$se,x1=barcenters,y1=b2.mean$mean+b2.mean$se,lwd=2)
# Error bars with whiskers
arrows(x0=barcenters,y0=b2.mean$mean-b2.mean$se,x1=barcenters,y1=b2.mean$mean+b2.mean$se,lwd=2, 
       angle=90, length=0.05,code=3)


##############################

## Add text annotations above bars
?text()

# Define x,y coordinates of annotation by hand:
text(x=1.5,y=29,"a",cex=1.3)
text(x=2.5,y=30,"a",cex=1.3)

# Or define x,y automatically 
barplot(b2.mean2,ylim=c(0,35),col=rep("white",3),yaxt='n',mgp=c(3,3,0),font=2,beside=T)
barplot(b2.mean2,names.arg=rep(c("2009","2010","2011"),2),ylim=c(0,35),
        col="black",ylab="Time (days)", main="Development time", cex.lab=1.2,
        angle=barangle,density=bardensity,mgp=c(3,1,0),beside=T,add=T)
arrows(x0=barcenters,y0=b2.mean$mean-b2.mean$se,x1=barcenters,y1=b2.mean$mean+b2.mean$se,lwd=2, 
       angle=90, length=0.1,code=3)
# x coordinates were already calculated in "barcenters"
# y coordinates from the dataframe "b2.mean"
text(x=c(barcenters[,1],barcenters[,2]),
     y=b2.mean[,2]+4,
     labels=c("a","a","b","c","b","d"),cex=1.3)

##############################

## Interrupted y axis
# Say we want to remove the axis segment between 15 and 20
# One way to do this:
#1. adjust the values in your dataframe > 15 by subtracting the segment of 5
#2. assign the correct values on the axis
#3. show an interruption sign on the axis with axis.break (package {plotrix})

# We only use data of species 1
sp1 <- b2.mean2[,1]
# which values >15?
sp1>15
# Adjust values >15
sp1b <- ifelse(sp1>15,sp1-5,sp1)
sp1b
sp1
# Plot bars
par(mfrow=c(1,2))
barplot(sp1b,ylim=c(0,30),col=rep("white",3),yaxt='n',xaxt='n',mgp=c(3,3,0),font=2,beside=T)
barplot(sp1b,ylim=c(0,30),names.arg=c("2009","2010","2011"),yaxt='n',
        col="black",main="Development time", cex.lab=1.2,
        angle=barangle,density=bardensity,mgp=c(3,1,0),beside=T,add=T)
# Plot interrupted y axis (labels= correct values to show, at= at position of values of your adjusted dataframe sp1b)
axis(2,ylim=c(0,30),labels=c(0,5,10,25),at=c(0,5,10,20))
axis.break(2,breakpos=15,style="slash",brw=0.03)
mtext(2,text="Time (days)",line=2,font=2)

# For comparison: the plot with normal y-axis
barplot(sp1,ylim=c(0,30),col=rep("white",3),xaxt='n',yaxt='n',font=2,beside=T)
barplot(sp1,ylim=c(0,30),names.arg=c("2009","2010","2011"),
        col="black",main="Development time", ylab="Time (days)", cex.lab=1.2,
        angle=barangle,density=bardensity,beside=T,add=T)


########################
# Stacked barplot

## Stacked barplot: 3 years stacked per species
barangle <- c(135,45,0)
bardensity <- c(12,30,0)
par(mfrow=c(1,1),lwd=2)

# First barplot with background colour
barplot(b2.mean2[,1:2],col=rep("white",3),yaxt='n',xaxt='n',beside=F)
# Add dashed lines and titles with "add=T" 
barplot(b2.mean2[,1:2],ylab="Time (days)", main="Development time", 
        cex.lab=1.2, col="black",angle=barangle,density=bardensity,add=T)
# Add legend
legend("topright",c("2009","2010","2011"),fill=rep("white",3),horiz=F,bty="n")
legend("topright",c("2009","2010","2011"),angle=barangle,density=bardensity,horiz=F,bty="n")

# Change legend in same order as in the bars
# using rev() 
barplot(b2.mean2[,1:2],col=rep("white",3),yaxt='n',xaxt='n',beside=F)
barplot(b2.mean2[,1:2],ylab="Time (days)", main="Development time", 
        cex.lab=1.2, col="black",angle=barangle,density=bardensity,add=T)
legend("topright",rev(c("2009","2010","2011")),fill=rep("white",3),horiz=F,bty="n")
legend("topright",rev(c("2009","2010","2011")),angle=rev(barangle),density=rev(bardensity),horiz=F,bty="n")


################################################

## Line plots

# Add again the year column in the matrix
year <- c(2009,2010,2011)
b2.mean2 <- cbind(b2.mean2,year)
b2.mean2

# Plot species 1
plot(x=b2.mean2[,3],y=b2.mean2[,1], axes=F, xlab="", ylab="",type="l",
     main="Development time",col="black",ylim=c(0,35))
points(x=b2.mean2[,3],y=b2.mean2[,1],pch=19,col="black")
# Add species 2
par(new=TRUE)
plot(x=b2.mean2[,3],y=b2.mean2[,2], axes=F, xlab="", ylab="",type="l",col="black",lty=2,ylim=c(0,35))
points(x=b2.mean2[,3],y=b2.mean2[,2],pch=21,col="black")
# Add axes
axis(2, ylim=c(0,35),col="black",lwd=2)
mtext(2,text="Time (days)",line=2,font=2)
axis(1, xlim=c(2009,2011),col="black",lwd=2,xaxp=c(2009,2011,2))
mtext(1,text="Year",line=2,font=2)

## With secondary y-axis and error bars
# Increase right margin for 2nd y-axis
par(mar=c(5,4,4,8))

# Plot species 1
plot(x=b2.mean2[,3],y=b2.mean2[,1], axes=F, xlab="", ylab="",type="l",
     main="Development time",col="black",ylim=c(0,35))
points(x=b2.mean2[,3],y=b2.mean2[,1],pch=19,col="black")
axis(2, ylim=c(0,35),col="black",lwd=2)
mtext(2,text="Species 1 (days)",line=2,font=2)
axis(1, xlim=c(2009,2011),col="black",lwd=2,xaxp=c(2009,2011,2))
mtext(1,text="Year",line=2,font=2)
# Error bars with whiskers
arrows(x0=b2.mean2[,3], y0=b2.mean2[,1]-b2.mean[1:3,4],x1=b2.mean2[,3],y1=b2.mean2[,1]+b2.mean[1:3,4],lwd=2, 
       angle=90, length=0.1,code=3)

# Add species2 with new y-axis using a different scale
par(new=TRUE)
plot(x=b2.mean2[,3],y=b2.mean2[,2], axes=F, xlab="", ylab="",type="l",col="black",lty=2,ylim=c(0,25))
points(x=b2.mean2[,3],y=b2.mean2[,2],pch=21,col="black")
axis(4, line=2,ylim=c(0,25),col="black",lwd=2)
mtext(4,line=4,text="Species 2 (days)",font=2)
# Error bars with whiskers
arrows(x0=b2.mean2[,3],y0=b2.mean2[,2]-b2.mean[4:6,4],x1=b2.mean2[,3],y1=b2.mean2[,2]+b2.mean[4:6,4], lwd=2, 
       angle=90, length=0.1,code=3)


###################### THE END ##########################
# Acknowledgements: Lidwien Raak, Lia Hemerik, Jenny Lazebnik, and Xi Cheng

## Packages for making plots:  
# sciplot - bar and line plots without having to calculate means and st.errors yourself
# ggplot2 - lots of fancy options
# ...
