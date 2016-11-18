## Script for R user group, October 19, 2016, Wageningen University & Research 
##  "Playing around with graphs":  
##  easily explore (distribution) functions, a dynamic function and create an animation
## By Luc Steinbuch

## 1. Explore distribution and other functions ####
curve(dnorm)

curve(dnorm, from = -2, to = 2)

curve(dnorm(x=x, sd = 0.5), from = -2, to = 2)

# a bit more effort for discrete functions
curve(dpois(x=x, lambda = 3), from = 0, to = 10, n=11, type="p")
grid()

# smart with; inf create your own function
curve(1/x, from = -2, to = 2)
grid()

# create your own somewhat bigger function, and show it
logit_squared <- function(p)
{
    y <- log(p/(1-p))
    y <- y^2
    return(y)  
}
curve(logit_squared(x))
grid()

# always a good idea to clean up your mess ;-)
remove(logit_squared)

## compare distributions 
curve(dnorm(x=x, mean=0, sd=1), from = -2, to = 2)
curve(dt(x=x, df=5), add=TRUE, col="blue", lty="dashed")
title("t with df=5 vs standard normal")
grid()

## and go cumulative 
curve(pnorm(q=x, mean=0, sd=1), from = -2, to = 2)
curve(pt(q=x, df=5), from = -2, to = 2, add=TRUE, col="blue", lty="dashed")
title("t with df=5 vs standard normal")
grid()


#2. Interactive exploration - as example: soil water retention ####
# only works in Rstudio :-(

# Background info:
browseURL("https://en.wikipedia.org/wiki/Water_retention_curve")
# What is the relation between soil water content and the water 'suction pressure'?
# We use the van Genuchten model, with 4 parameters

# Load dataset
df_water_retention <-
structure(list(observed_pressure = c(4.025, 7.075, 7.105, 10, 
10, 10.15, 30, 58.84, 83.035, 107.895, 140.795, 178.715, 220.78, 
333.71, 486.05, 503.32, 518.335, 3000, 3000, 15000), observed_water_content = c(0.466240941, 
0.460427966, 0.460116832, 0.44, 0.4314, 0.455052989, 0.416, 0.422555985, 
0.413997431, 0.409623935, 0.402093289, 0.398001156, 0.392574042, 
0.380885402, 0.368482698, 0.367229252, 0.366159469, 0.2471, 0.2545, 
0.1721), modelled_water_content = c(NA, NA, NA, NA, NA, NA, NA, 
NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)), .Names = c("observed_pressure", 
"observed_water_content", "modelled_water_content"), row.names = c(NA, 
-20L), class = "data.frame")

# short look
plot(x    = df_water_retention$observed_pressure,
     y    = df_water_retention$observed_water_content,
     log  = "x", # x scale logaritmic
     xlab = "pressure [hPa]",      
     ylab = "water content [m3/m3]", 
     main = "Water retention curve - observation data, sample Bun12",
     sub  = "note: x and y is often reversed in literature"
     )

# Is a soil close to village of Bunnik, so probably river clay; 
# with thanks to Wageningen University & Research (former Alterra)

# We will need model input for line plotting: a vector of pressures. 
# looks complicated because we want to cover logaritmic scale
vn_model_pressure = c( seq(1,16,length.out=10) %o% 10^(0:3) )
vn_model_pressure <- sort(vn_model_pressure)
vn_model_pressure



# the following function creates a van Genuchten Model (given parameters) and plots the results

show_vG <- function(nGs, nGr, nGa, nGn, bLogScale)
{

  ## the van Genuchten model, for the lineplot
  vn_model_moisture <- nGr + (nGs-nGr)/(1+(nGa*vn_model_pressure)^nGn)^(1-1/nGn) 
  # for programmers: note that vn_model_pressure is available within the scope of this function
  
  ## the van Genuchten model, for the pointplot - for the same pressures as the observations
  df_water_retention$modelled_water_content <- 
    nGr + (nGs-nGr)/(1+(nGa*df_water_retention$observed_pressure)^nGn)^(1-1/nGn) 
    
  
  #calculate residual sum of squares as a measure for 'goodness of fit'
  n_res_ss <- sum( (df_water_retention$observed_water_content-df_water_retention$modelled_water_content)^2)
  
  # is the x scale logaritmic or not?
  if (bLogScale)
  {
    strLog <- "x"
  } else
    strLog <- ""  
    
  strSub = paste("residual Sum of Squares = ", format(n_res_ss, digits=3) ) 
  
  # plot the model outcomes as line (full scale)
  plot(x    = vn_model_pressure, 
       y    = vn_model_moisture, 
       type = "l",   # a line plot; this is an l, not a one!   
       log  = strLog,
       main = "Water retention curve - observations and model",
       sub  = strSub,
       xlab = "pressure [hPa]",
       ylab = "moisture content [m3/m3]",
       ylim = c(0, 1),
       col  = "blue"
  )
  
  # add to the  plot the observations
  points(x = df_water_retention$observed_pressure, 
         y = df_water_retention$observed_water_content, 
         col="red", 
         pch=8,# the shape of the point symbol, 
         # browseURL("http://vis.supstat.com/2013/04/plotting-symbols-and-color-palettes/")
         cex=1)  # the size of the symbol
  
  # add to the plot the modelled water content on the value of the observed pressures
  points(x = df_water_retention$observed_pressure, 
         y = df_water_retention$modelled_water_content, 
         col="blue", 
         pch=5,
         cex=1)    
  grid()
  
}



#If needed:  install.packages("manipulate")
require(manipulate)


manipulate(show_vG (Gs, Gr, Ga, Gn,  LogSCale),
           Gs = slider(0.3,   1,  initial=0.6, label = "Saturated soil water content"),
           Gr = slider(0,    0.3, initial=0.1, label = "Residual soil water content"),
           Ga = slider(0,   0.05, initial=0.025, step=0.001, label = "alpha: 'Inverse of air entry suction'"),
           Gn = slider(1,   2.5,  initial=1.2,   step=0.05,  label = "n: 'Pore-size distribution'"),
           LogSCale = checkbox(initial = TRUE, label = "Plot log pressure head")
)



remove(list=ls())   # remove all variables and self-defined functions



###### 3. Animation of plots - as example: visualize space time process####


#### Load data Northern Los Angeles County Fires 

df_fires <-
structure(list(Time = c(5863L, 5870L, 6017L, 6018L, 6034L, 6060L, 
6176L, 6364L, 6366L, 6372L, 6383L, 6387L, 6406L, 6414L, 6415L, 
6415L, 6487L, 6739L, 6742L, 6763L, 6764L, 6771L, 6772L, 6784L, 
6870L, 7040L, 7067L, 7085L, 7090L, 7098L, 7099L, 7103L, 7107L, 
7109L, 7116L, 7122L, 7124L, 7128L, 7129L, 7131L, 7131L, 7132L, 
7135L, 7135L, 7135L, 7137L, 7138L, 7193L, 7194L, 7194L, 7195L, 
7195L, 7197L, 7199L, 7201L, 7285L, 7289L, 7406L, 7448L, 7455L, 
7464L, 7472L, 7475L, 7479L, 7485L, 7490L, 7491L, 7492L, 7494L, 
7495L, 7496L, 7499L, 7501L, 7502L, 7502L, 7502L, 7502L, 7506L, 
7508L, 7508L, 7513L, 7513L, 7513L, 7516L, 7520L, 7549L, 7566L, 
7568L, 7575L, 7580L, 7597L, 7606L, 7626L, 7626L, 7636L, 7649L, 
7800L, 7818L, 7834L, 7835L), X = c(63.92616, 64.29335, 64.143, 
63.97239, 64.44143, 64.14625, 66.23951, 65.45876, 65.33066, 66.47225, 
66.37243, 64.81267, 64.69134, 65.24686, 64.2917, 65.16721, 64.28823, 
65.75098, 65.05372, 66.0153, 64.67382, 64.33003, 65.04228, 64.54461, 
64.56109, 64.58657, 64.45742, 65.01338, 64.94648, 64.46145, 64.32155, 
64.76439, 63.92878, 64.37877, 64.39877, 64.48427, 64.56856, 64.37879, 
64.03954, 64.02836, 63.73284, 63.86294, 65.55396, 65.31792, 64.44186, 
64.6555, 63.86445, 64.91534, 64.71286, 64.72915, 65.38518, 65.08401, 
64.91035, 64.51824, 64.0869, 64.22628, 64.23695, 64.58552, 63.82293, 
63.81666, 64.50737, 64.06718, 65.50382, 65.46496, 64.0692, 65.6448, 
64.39534, 64.98099, 64.17322, 64.78116, 64.47019, 64.28615, 64.66565, 
65.43439, 64.81091, 64.38863, 64.42881, 64.23128, 64.63139, 64.81866, 
65.36119, 65.82657, 65.34735, 64.49847, 64.78275, 65.43803, 64.03015, 
64.27501, 64.13717, 65.29844, 64.24639, 64.41798, 63.94346, 64.45589, 
63.98245, 63.93946, 64.22573, 65.2377, 64.29272, 63.75178), Y = c(19.35161, 
20.11176, 19.69055, 19.82465, 20.27411, 19.71447, 19.41608, 19.99348, 
20.06103, 20.26253, 20.29229, 20.34431, 20.43665, 19.54616, 20.69978, 
20.08614, 20.04276, 20.69676, 20.05262, 19.84342, 19.90643, 20.09656, 
20.04107, 20.25063, 20.02257, 19.5683, 19.84837, 19.88626, 20.00191, 
19.8278, 20.02379, 20.91297, 19.71967, 19.89217, 19.35751, 20.05932, 
19.90473, 20.0718, 19.30933, 19.78133, 19.90342, 19.65069, 19.90812, 
20.13764, 20.02264, 20.05204, 19.875, 19.96295, 19.97626, 19.93319, 
19.47538, 19.43762, 19.86542, 19.34165, 19.47651, 19.57369, 19.97179, 
19.56545, 20.18196, 20.18377, 20.91476, 19.92259, 20.29662, 20.24826, 
20.25685, 19.95193, 19.71675, 20.0126, 19.42019, 20.41243, 19.8404, 
20.03391, 20.75252, 20.06062, 21.04297, 19.84934, 19.99799, 19.80955, 
20.72933, 20.9931, 20.8474, 20.48923, 20.8206, 20.87301, 21.02026, 
19.9707, 19.92486, 20.03034, 20.35587, 19.95439, 20.11622, 19.31176, 
19.97367, 19.94748, 19.3505, 19.34459, 19.86676, 20.1716, 19.77528, 
19.98862)), .Names = c("Time", "X", "Y"), row.names = c(NA, 100L
), class = "data.frame")


# More info:
browseURL("http://artax.karlin.mff.cuni.cz/r-help/library/spacetime/html/fires.html")
# We have here only the fist 100 observations. See the appendix how to get the complete dataset

View(df_fires)

#  install.packages("animation")
require(animation)



# The function saveHTML in the animation package  will create 100 plots and shows 
# those plots in one animation. All the plots should have the same x and the same y axis; 
# that's what we calculate here
vn_x_limits <- c(min(df_fires$X), max(df_fires$X))
vn_y_limits <- c(min(df_fires$Y), max(df_fires$Y))

# initialize vector, to avoid error later
vc_fires <- NA

saveHTML({
  for (i in 1:100)
  {            
    
    # 100 plots are generated; the first plot shows the first fire, the second
    # plot the first two fires etc etc.
    # To get a fading-out effect, a vector of changing colors is constructed alongside the plots
    vc_fires [i] <- rgb(red=1, green=0, blue=(i/100), alpha=1-(i/110) )
    
    plot(x    = df_fires$X[1:i], 
         y    = df_fires$Y[1:i], 
         xlim = vn_x_limits, 
         ylim = vn_y_limits,
         col  = rev(vc_fires),  # reverse the vector of colors: 
         # the most recent fire has the most opaque color
         pch  = 8,  
         xlab = "E/W coordinate",
         ylab = "N/S coordinate",
         main   = paste("Fires until ", as.Date("1960-01-01")+(df_fires$Time[i]-1)) 
        )
  }  
  
  
  }, 
  img.name    = "plot", 
  imgdir      = "images01", 
  htmlfile    = "example.html", 
  autobrowse  = TRUE,   # start internet browser
  title       = "Animation example R: Northern Los Angeles County Fires", 
  loop        = FALSE, # no infinite loop
  verbose     = FALSE, # not showing extra information
  interval    = 0.2  # starting speed of animation
)


# I used http://gifmaker.me/ to create an video for ms-powerpoint presentation; note that the 
# different plots are stored as .png images

# however, watch out for viruses..



##### Appendix: loading the Northern Los Angeles County Fires dataset #####

# install.packages("spacetime")
require(spacetime)

# what example data  is with this package?
data(package='spacetime')

?fires

# load 'fires' 
data(fires)

# matter of preference: show explicitly that it is a dataframe
df_fires <- fires
remove(fires)


# First 100 observations (=rows) only 
df_fires <- df_fires[1:100,]

# the dataset as R code shown above:
rownames(df_fires) <- NULL # remove rownames, because obsolete
dump("df_fires", file="")





