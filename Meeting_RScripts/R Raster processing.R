

#### libraries ####

library(sp)
library(raster)

# wrapper functions for GDAL command line utilities
# requires working installation of GDAL
# simple way to get GDAL: 
# install QGIS, during which GDAL will be installed as well + system variables/bashcr will be modified
library(gdalUtils)
# from experience: gdalUtils (C-based) faster than raster on larger rasters (file-based) than raster package (R-based)

# experimental C++ accelerated raser processing
library(velox)

tmp <- 'D:/Temp/raster/'

#### raster I/O ####

b <- brick(system.file("external/rlogo.grd", package="raster"))
plot(b)
r <- subset(b, 1)
plot(r)

# suffix determines data format
writeRaster(b, file.path(tmp, 'rlogo.tif'))

#### meta data ####

# raster package
b

# gdalUtils
gdalinfo(file.path(tmp, 'rlogo.tif'))


#### introduce NAs ####

# always take care of proper handling
b[sample(1:ncell(b), 20)] <- NA
r <- subset(b, 1)


#### geo-operations: resample ####

r_template <- aggregate(r, 4)
r_res <- resample(r, r_template)
plot(r_res)

# gdalwarp(srcfile = 'source.tif', dstfile = 'target.tif', tr = c(30, 30))


#### geo-operations: reproject ####

r_proj <- projectRaster(r, crs = '+init=epsg:4326')
extent(r_proj) <- extent(c(xmin = 5, xmax = 6, ymin = 51.75, ymax = 53.25))
writeRaster(r_proj, file.path(file.path(tmp, 'rlogo_WGS84.tif')))

# gdalUtils
gdalwarp(srcfile = file.path(file.path(tmp, 'rlogo_WGS84.tif')), 
         dstfile = file.path(file.path(tmp, 'rlogo_UTM.tif')), 
         t_srs = '+init=epsg:32631')


#### processing: calculations ####

# raster algebra
r_plus1 <- r + 1
r_plus2 <- calc(r, function(x) x + 1)
plot(r_plus1)
all.equal(r_plus1, r_plus2)

# mult-layer
b_sum1 <- sum(b)
b_sum2 <- calc(b, sum)
all.equal(b_sum1, b_sum2)


#### processing: focal ####

# moving window approach - neighborhood relationships

# e.g. fill NA wholes
r_filled <- focal(r, w = matrix(1, ncol = 3, nrow = 3), fun = mean, na.rm = TRUE, NAonly = TRUE, pad = TRUE)
plot(r_filled)
all.equal(r_filled, r)





