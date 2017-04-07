### ggplot2 + ###

# load library ####
library(ggplot2)
# datasets
?mpg
View(mpg)
?iris
View(iris)

# create the frame - ONE VARIABLE ####
g <- ggplot(data = mpg, aes(x = cty))
g
# now, add layers
g + geom_density()
g + geom_histogram()
g + geom_histogram(bins = 10, aes(y= ..density..))
g + geom_histogram(bins = 10, aes(y= ..density..)) + geom_density()
g + geom_histogram(bins = 10, aes(y= ..density..)) + geom_density() 
g + geom_histogram(bins = 10) + geom_density(aes(y = ..count..*3))

# create the frame - TWO VARIABLE ####
g <- ggplot(data = mpg, aes(y = cty, x = hwy))
g + geom_point()
g + geom_point(aes(color = year), alpha = 0.5, cex = 3)
g + geom_bin2d()
g + geom_hex()
# geom_point(position = "jitter"). 
# It adds a small amount of random variation to the location of each point, same than
g + geom_jitter()
g + geom_jitter(aes(color = year), alpha = 0.5, cex = 3)

# x = Discrete, y = Continuous
g <- ggplot(data = mpg, aes(y = cty, x = class))
g + geom_boxplot()
g + geom_boxplot(aes(fill = fl)) 
g + geom_boxplot(aes(fill = fl))

# Facets ####
library(reshape)
mpg <- as.data.frame(mpg)
mpg.t <- melt(data = mpg,
              id.vars = c("class", "year"),
              measure.vars = c("cty", "hwy"))

g <- ggplot(data = mpg.t, aes(x = class, y = value))
g + facet_grid(~ variable) 
g + facet_grid(~ variable) + geom_boxplot(aes(color = class))
g + facet_grid(~ variable + year) + geom_boxplot(aes(color = class))
g + facet_grid(variable ~ year) + geom_boxplot(aes(color = class))
g + facet_wrap(variable ~ year, scales = "free_y") +
  geom_boxplot(aes(color = class))
(gg <- g + facet_wrap(variable ~ year) + geom_boxplot(aes(fill = class)))

# Customizations ####
# change the colors
gg + scale_fill_grey()
# change font family
gg + theme(text = element_text(family = "serif")) 
# add title and labels
gg + labs(title = "Fuel economy of cars", x = "Class",
          y = expression("Consumption / km gal"^{-1})) +
  theme(text = element_text(family = "serif")) 

# Extra tools ####
# extra themes
library(ggthemes)
gg + theme_economist()

# Complex plots
require("GGally")
data(flea)
ggscatmat(flea, columns = 2:7, color="species", alpha=0.3)

# Interactive plots
require(plotly)
ggplotly(gg)
## Spatial plot ####
require(sp)
demo(meuse)

g <- ggplot(data = as.data.frame(meuse),
            mapping = aes(x = x, y = y, color = zinc, size = copper))  
g
g + geom_point() 
g + geom_point() + coord_equal()

# Other options to plot spatial data https://edzer.github.io/sp/

# save a plot as image ####
# change / by \ depending your operative system
png(filename = "/<replace with your folder name>/png.png",
    res = 300, width = 2000,height = 2000)
g + geom_point() + coord_equal()
dev.off()

# Excercercise ####
meuse <- as.data.frame(meuse)
str(meuse)
# labels
# dist = Distance to the river
# elev = Elevation
# soil = Soil type
# ffreq = Flod frequency

# 1) 
# we want to show in a figure how cadmium, copper, lead and zinc vary given 
# distance and elevation. Take into account that the scales of the heavy metals 
# are different among them.

g <- ggplot(...)

# 2)
# Now, we want to see the same, but also given soil type and or flod frequency.

g <- ggplot(...)

# 3) 
# Add title and labels to axes, and save as .png format image.



















# A hint for solutions ####
# define the data and mapping (x, y and one heavy metal as color or size)
g <- ggplot(data = meuse, 
            mapping = aes(x= , y =,  ))
# define the geom_ argument (point, hex, bind2d)
g + geom_point()
# You can use melt function (reshape2 package) to change the structure
# of your dataframe, such that: 

meuse.df <- reshape::melt(data = ,
                          id.vars = c("", "", ...),
                          measure.vars = c("", "", ...))
# check line number 42
# then, generate g again, such that
g + ggplot(data = meuse.df, aes(x = , y = , color = values))

g + facet_grid(soil ~ variable)


