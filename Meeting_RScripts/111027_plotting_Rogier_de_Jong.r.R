## ======================================================================
##
##   R User Group 27-10-2011: PLOTTING in R
##
##	Rogier de Jong
##	Laboratory of Geo-Information Science and Remote Sensing (WUR)
## http://www.rogierdejong.net | Rogier.deJong@wur.nl
##
## ======================================================================

## R GUI
# During the meeting I will use RStudio (http://www.rstudio.org) as R
# interface. Everything will work similar if you use Tinn-R or the 
# default R GUI.
#
## DATA
# In these examples I use time-series data (global temperature). The 
# functionality is all basic and should work with many types of data.
# Please bring global.dat (see Dropbox folder) 
#
# PACKAGES
# Please install 'nlme' and 'lattice'
#
# R REFERENCE CARD
# Very handy!
# http://cran.r-project.org/doc/contrib/Short-refcard.pdf
#


#-----------------------------------------------
# Let's get some data
#-----------------------------------------------

# 1800 monthly temperature anomalies (1856 until 2005)
# Data courtesy: Paul Cowpertwait

# download en read data (internet connection needed!)
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/global.dat"
Global <- scan(www)
# OR (without internet connection) read from file
# make sure that global.dat is in:
getwd()
Global <- scan("global.dat")

# make subsets per decade (each 120 monthly values)
Global40s <- Global[1009:1128]
Global50s <- Global[1129:1248]
Global60s <- Global[1249:1368]


#-----------------------------------------------
# 1: Plot types and building your own plot
#-----------------------------------------------

# try different plot-types:
plot(Global50s, type="p",col=4)
plot(Global50s, type="l", main="Global temperature anomalies")
plot(Global50s, type="b", xlab="months since 1950", ylab="anomaly (deg C)")
plot(Global50s, type="h")

# or customize your own plot from scratch
plot(Global50s, type="n", axes=F, xlab="", ylab="")
axis(1, at=c(0:10)*12, labels=c(0:10))# at define las divisiones que queres ahi se hacen divisiones cada 12 y van hasta 10 o sea 120 y labels son las labels q le pones a la divsion
axis(2)
abline(h=0, col="darkred", lty=4)# lty= line type
lines(Global40s, col="darkgrey", lty=1)
lines(Global50s, col="darkgreen", lty=2)
lines(Global60s, col="darkblue", lty=3)
mtext("Global temperature anomalies", side=3, font=2)# side es donde y font el tipo de letra.
mtext("time (yrs after start)", side=1, line=2.5)#line define que tan lejos del eje la leyenda
mtext("anomaly (deg. C)", side=2, line=2.5)
legend(x=90,y=-0.27,legend=c("1940s","1950s","1960s"),lty=c(1,2,3), col=c("black","darkgreen","darkblue"), bty="n") #bty means box-type , n es no box, el lugar de las leyendas, x y y es las mismas unidades que los datos (90 es 90/12)

## at: location of axis labels
## side: clockwise, bottom=1 left=2 top=3 right=4
## line: distance from axis
## font: font-style (2 = bold, 3 = itallic)
## lty: line type, see ?par
## bty: box/border type ("n" = none), see ?par


#! there are many package-specific plot functions which you can use
#! building your own plot from scratch is the way to have full control



#-------------------------------------------------
# 2: Fitting and displaying (regression) models
#-------------------------------------------------

# make time-series object (see ?ts for info)
Global.ts <- ts(Global, start = c(1856, 1), end = c(2005, 12), frequency = 12)
str(Global.ts)
x <- time(Global.ts)

# aggregate by year
Global.annual <- aggregate(Global.ts, FUN = mean)
plot(Global.annual)# en este caso entiende de que es un objeto que es serie de tiempo y te pone las labels

## smoothing (LOESS)
loess.fit <- loess(Global.ts ~ x, span=0.5)#span is alpha parameter degree of smoothing
loess.predict <- predict(loess.fit, x)
loess.ts <- ts(loess.predict, start = c(1856, 1), end = c(2005, 12), frequency = 12)
plot(Global.ts)
lines(loess.ts, col="blue")

## simple linear regression model
Global2 <- window(Global.ts, start=1970) # subset 1970-2005 window funciona como subset
Global2.lm <- lm(Global2 ~ time(Global2))#tira un modelo lineal donde parece mas lineal la curva
coef(Global2.lm) #coeficientes del modelo lineal (intercepto y slope)
plot(Global2)
abline(Global2.lm, col="blue", lty=4) # in this case abline tira una linea con los coeficientes del modelo lineal.
#Se puede graficar los intervalos de confianza (confint(lm)) podes poner los interceptos mas y menos los intervalos y graficas un linea por debajo y otro para arriba. Luego podes usar la funcion polygon() y te une los puntos y te hace como una superficie/


#! note: OLS cannot be used for significance tests, 
#! because of serial autocorrelation

## generalized least squares (GLS)
library(nlme)
Global2.gls <- gls(Global2 ~ time(Global2), cor=corAR1(0.75))
# cor = correlation structure, in this case autoregressive
coef(Global2.gls)
abline(Global2.gls, col="red", lty=4)



#-------------------------------------------------
# 3: Lattice & Layout: working with panels
#-------------------------------------------------

## multiple panels in a plot
l <- layout(matrix(c(1,2,3,4),4,1,byrow=TRUE))
layout.show(l)
l <- layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
layout.show(l)

#! on each occurence of plot() you switch to the next panel
#! reset to default: layout(1)
plot(Global, type="l")
plot(Global40s, type="l", main="1940s")
plot(Global60s, type="l", main="1960s")

# using multiple panels you often want to change the margins:
par(mar = c(0, 4, 0, 2)) # clockwise from bottom

# example:
l <- layout(matrix(c(1,2,3),3,1,byrow=TRUE), heights=c(1,1,1.25))
layout.show(l)
plot(Global40s, type="l", main="", axes=F)
   axis(2)
   abline(lm(Global40s ~ c(1:120)), col="blue", lty=4)
plot(Global50s, type="l", main="", axes=F)
   axis(2)
   abline(lm(Global50s ~ c(1:120)), col="blue", lty=4)
par(mar = c(4, 4, 0, 2)) # set extra bottom margin
plot(Global60s, type="l", main="", axes=F)
   axis(1)
   axis(2)
   abline(lm(Global60s ~ c(1:120)), col="blue", lty=4)


## multi-panel plotting per (nominal / ordinal) group: 
## lattice (trellis-plots)
library(lattice)

# make a data frame with:
# - observation number (x)
# - temperature anomaly (TA)
# - group number (1-6)
Group <- round(runif(length(Global.annual),0.5,6.5),0) # random
Group <- round(c(1:150)/25+0.4999,0) # 111 222 333 444 555 666 

Global.df <- data.frame("X" = c(1:length(Global.annual)), "TA" = Global.annual, "group" = Group)
str(Global.df)

# scatterplot
xyplot(TA ~ X | group, data=Global.df, type="l")
# boxplot
bwplot(group ~ TA, data=Global.df)

## advanced lattice: customizing panels and strips
myPanel <- function(x, y, g, ...) {
  panel.xyplot(x,y,col=rgb(0,100,0,30,maxColorValue=255), pch=46)
  panel.bwplot(x,y)
  panel.grid(v=-1, h=0)
}
Strip.labels <- c("1st Period","2nd Period","3rd Period","4th Period","5th Period","6th Period")
myStrip <- function(which.panel, ...) {
  llines(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0),col="black")
  ltext(0.5, 0.5, Strip.labels[which.panel], col="black") 
}
tplot <- xyplot(TA ~ X | group, data=Global.df, strip=myStrip, panel=myPanel, layout=c(3,2), between=list(y=1), pch=46, main="My Trellis plot", xlab="Time", ylab="Temp anomalies")
print(tplot)

#! This is just an example, it takes a while to understand how
#! lattice plots work in detail


# ================END SCRIPT====================
