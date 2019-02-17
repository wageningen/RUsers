### ggplot2 + ###

# load library ####
library(ggplot2)
# datasets
?mpg
View(mpg)
?iris
View(iris)

ggplot(mpg, aes(hwy, cty)) +
    geom_point(aes(colour=cyl)) +
    geom_smooth(method="lm") +
    coord_cartesian() +
    scale_colour_gradient() +
    theme_bw()

# create the frame - ONE VARIABLE ####
g <- ggplot(data = mpg, aes(x = cty))
g
# now, add layers
g + geom_density()
g + geom_histogram()
g + geom_histogram(bins = 10, aes(y= ..density..))
g + geom_histogram(bins = 10, aes(y= ..density..)) + geom_density()
g + geom_histogram(bins = 10) + geom_density(aes(y = ..count..*3))

# create the frame - TWO VARIABLE ####
g <- ggplot(data = mpg, aes(y = cty, x = hwy))
g + geom_point()
g + geom_point(aes(color = year), alpha = 0.5, cex = 3)
g + geom_point(aes(color = as.factor(year)), alpha = 0.5, cex = 3)
g + geom_bin2d()
g + geom_hex()
# geom_point(position = "jitter"). 
# It adds a small amount of random variation to the location of each point, same than
g + geom_jitter()
g + geom_jitter(aes(color = as.factor(year)), alpha = 0.5, cex = 3)

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
          y = expression("Consumption / miles gal"^{-1})) +
  theme(text = element_text(family = "serif")) 

# Extra tools ####
# extra themes
require(ggthemes)
gg + theme_economist()

# Complex plots
require("GGally")
data(flea)
str(flea)
ggscatmat(flea, columns = 2:7, color="species", alpha=0.3)

# Interactive plots
require(plotly) #thanks Martin Mulder
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

# sf
require(sf)
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(nc)
ggplot(nc, aes(fill=AREA)) + geom_sf()

# ggmap
require(ggmap)
state = st_bbox(nc)
names(state) = c("left", "bottom", "right", "top")
us_map = get_stamenmap(state, zoom = 7, maptype = "toner-lite")
ggmap(us_map)

# sf + ggmap
# Bug: https://github.com/dkahle/ggmap/issues/160 Workaround:
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

nc_wgs84 = st_transform(nc, 3857)
ggmap(ggmap_bbox(us_map)) + geom_sf(data=nc_wgs84, aes(fill=AREA), alpha=0.5, inherit.aes = FALSE)


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








# Possible solutions ####
# 1) 
# we want to show in a figure how cadmium, copper, lead and zinc vary given 
# distance and elevation. Take into account that the scales of the heavy metals 
# are different among them.

g <- ggplot(data = meuse, aes(x = dist, y = elev,
                                 color = cadmium, size = copper))
g + geom_point()

# 2)
# Now, we want to see the same, but also given soil type and or flod frequency.

g + geom_point() + facet_grid(~soil)
g + geom_point() + facet_grid(~ffreq)
g + geom_point() + facet_grid(~soil + ffreq)
g + geom_point() + facet_grid(soil ~ ffreq)

meuse.df <- reshape::melt(data = meuse,
                          id.vars = c("soil", "ffreq", "dist", "elev"),
                          measure.vars = c("cadmium", "copper", "lead", "zinc"))

g <- ggplot(data = meuse.df, aes(x = dist, y = value, color = elev))

gg <- g + facet_wrap(soil ~ variable,scales = "free_y") +
  geom_point() 
gg
# 3) 
# Add title and labels to axes, and save as .png format image.

png(filename = "/home/marcos/Desktop/png.png",
    res = 300, width = 2500,height = 2000)
gg + labs(title = "Heavy metals near Meuse river",
          x = "Distance to the river",
          y = "Concentration")
dev.off()
