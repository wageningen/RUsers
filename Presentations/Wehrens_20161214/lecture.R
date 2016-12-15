## ----knitrInitialization,echo=FALSE--------------------------------------
require("knitr", quietly=TRUE)
opts_chunk$set(comment=NA, background='transparent', size = 'scriptsize', fig.width=6, fig.height=6, out.width='\\textwidth')

require(latticeExtra, quietly = TRUE)
require(gridExtra, quietly=TRUE)      

## ----mtcarsBaseCode------------------------------------------------------
data(mtcars)
class(mtcars)
dim(mtcars)

## ----mtcarsPrint---------------------------------------------------------
head(mtcars[,1:4])

## ----mtcarsCylPlot,eval=FALSE,echo=TRUE----------------------------------
#  ncyl <- c(8, 6, 4)
#  par(mfrow = c(3,1))
#  
#  for (nc in ncyl) {
#    idx <- which(mtcars$cyl == nc)
#  
#    plot(density(mtcars$mpg[idx]),
#         main = nc,
#         xlab = "Miles per Gallon",
#         ylab = "Density")
#    rug(mtcars$mpg[idx], ticksize = .1)
#  }

## ----mtcarsCylPlot2,echo=FALSE,eval=TRUE,ref.label='mtcarsCylPlot',fig.height=6,fig.width=4----
ncyl <- c(8, 6, 4)
par(mfrow = c(3,1))

for (nc in ncyl) {
  idx <- which(mtcars$cyl == nc)
  
  plot(density(mtcars$mpg[idx]),
       main = nc,
       xlab = "Miles per Gallon",
       ylab = "Density")
  rug(mtcars$mpg[idx], ticksize = .1)
}

## ----mtcarsLattice,eval=FALSE--------------------------------------------
#  mtcars$cyl.f <- factor(mtcars$cyl)
#  densityplot(~ mpg | cyl.f,
#    data = mtcars,
#    xlab = "Miles per Gallon",
#    layout = c(1,3))

## ----mtcarsLatticePlot,ref.label='mtcarsLattice',echo=FALSE,fig.height=6,fig.width=4----
mtcars$cyl.f <- factor(mtcars$cyl)
densityplot(~ mpg | cyl.f,
  data = mtcars,
  xlab = "Miles per Gallon",
  layout = c(1,3))

## ----mtcarsLattice2,eval=FALSE-------------------------------------------
#  densityplot(~ mpg | cyl,
#    data = mtcars,
#    xlab = "Miles per Gallon",
#    layout = c(1,3))

## ----mtcarsLatticePlot2,ref.label='mtcarsLattice2',echo=FALSE,fig.height=6,fig.width=4----
densityplot(~ mpg | cyl,
  data = mtcars,
  xlab = "Miles per Gallon",
  layout = c(1,3))

## ----postdocData,eval=TRUE,echo=TRUE-------------------------------------
data(postdoc, package = "latticeExtra")
rownames(postdoc)
colnames(postdoc)

## ----postdocPlot,echo=FALSE,fig.width=9, fig.height=5--------------------
dotplot(prop.table(postdoc, margin = 1), as.table = TRUE,
        groups = FALSE, xlab = "Proportion",
        par.strip.text = list(abbreviate = TRUE, minlength = 10))

## ----postdocPlot2,echo=FALSE,fig.width=6, fig.height=8-------------------
dotplot(prop.table(postdoc, margin = 1), as.table = TRUE,
        layout = c(1,5), origin = 0,
        index.cond = function(x, y) median(x),
        xlab = "Proportion", aspect = .6, groups = FALSE,
        par.strip.text = list(abbreviate = TRUE, minlength = 10),
        scales = list(y = list(relation = "free", rot = 0)),
        prepanel = function(x, y) {
          list(ylim = levels(reorder(y, x)))
        },
        panel = function(x, y, ...) {
          panel.dotplot(x, reorder(y, x), ...)
        })

## ----standardPlots,echo=FALSE--------------------------------------------
pdf(file = "bwplot.pdf", width = 5, height = 5)
bwplot(voice.part ~ height, singer,
       panel = function(..., box.ratio) {
         panel.violin(..., col = "transparent",
                      varwidth = FALSE, box.ratio = box.ratio)
         panel.bwplot(..., fill = NULL, box.ratio = .1)
       } )
woppa <- dev.off()
pdf(file = "densityplot.pdf", width = 5, height = 5)
set.seed(36872)
rln <- rlnorm(100)
densityplot(rln, 
            scales = list(x = list(log = 2), alternating = 3),
            xlab = "Simulated lognormal variates",
            xscale.components = function(...) {
              ans <- xscale.components.default(...)
              ans$top <- ans$bottom
              ans$bottom$labels$labels <- parse(text = ans$bottom$labels$labels)
              ans$top$labels$labels <-
                if (require(MASS,quietly=TRUE))
                  fractions(2^(ans$top$labels$at))
                else
                  2^(ans$top$labels$at)
              ans
            })
woppa <- dev.off()
pdf(file = "barchart.pdf", width = 5, height = 5)
barchart(Titanic, scales = list(x = "free"),
         auto.key = list(title = "Survived"))
woppa <- dev.off()
pdf(file = "dotplot.pdf", width = 5, height = 7)
dotplot(variety ~ yield | site, data = barley, groups = year,
        key = simpleKey(levels(barley$year), space = "right"),
        xlab = "Barley Yield (bushels/acre) ",
        aspect=0.5, layout = c(1,6), ylab=NULL)
woppa <- dev.off()
pdf(file = "stripplot.pdf", width = 5, height = 5)
stripplot(voice.part ~ jitter(height), data = singer, aspect = 1,
          jitter.data = TRUE, xlab = "Height (inches)")
woppa <- dev.off()
pdf("histogram.pdf", width = 5, height = 7)
histogram( ~ height | voice.part, data = singer, nint = 17,
          endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
          xlab = "Height (inches)")
woppa <- dev.off()
pdf("qqmath.pdf", width = 5, height = 7)
qqmath(~ height | voice.part, data = singer,
       prepanel = prepanel.qqmathline, layout = c(2,4),
       panel = function(x, ...) {
          panel.qqmathline(x, ...)
          panel.qqmath(x, ...)
       })
woppa <- dev.off()

## ----quakesPlotCode0,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long,
#     data = quakes,
#     aspect = "iso")

## ----quakesPlot0,ref.label='quakesPlotCode0',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long, 
   data = quakes,
   aspect = "iso")

## ----quakesPlotCode1,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long,
#     data = quakes,
#     groups =  cut(mag, 3),
#     aspect = "iso")

## ----quakesPlot1,ref.label='quakesPlotCode1',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long, 
   data = quakes,
   groups =  cut(mag, 3),
   aspect = "iso")

## ----quakesPlotCode2,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long | cut(mag, 3),
#     data = quakes,
#     groups =  cut(mag, 3),
#     aspect = "iso")

## ----quakesPlot2,ref.label='quakesPlotCode2',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long | cut(mag, 3), 
   data = quakes,
   groups =  cut(mag, 3),
   aspect = "iso")

## ----quakesPlotCode3,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long | cut(mag, 3),
#     data = quakes,
#     groups =  cut(mag, 3),
#     aspect = "iso",
#     auto.key =
#       list(text = c("L", "M", "H"),
#            space = "top"))

## ----quakesPlot3,ref.label='quakesPlotCode3',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long | cut(mag, 3), 
   data = quakes,
   groups =  cut(mag, 3),
   aspect = "iso",
   auto.key = 
     list(text = c("L", "M", "H"),
          space = "top"))

## ----quakesPlotCode4,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long | cut(mag, 3),
#     data = quakes,
#     groups =  cut(mag, 3),
#     aspect = "iso",
#     as.table = TRUE,
#     auto.key =
#       list(text = c("L", "M", "H"),
#            space = "top"))

## ----quakesPlot4,ref.label='quakesPlotCode4',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long | cut(mag, 3), 
   data = quakes,
   groups =  cut(mag, 3),
   aspect = "iso",
   as.table = TRUE,
   auto.key = 
     list(text = c("L", "M", "H"),
          space = "top"))

## ----quakesPlotCode5,eval=FALSE,echo=TRUE--------------------------------
#  xyplot(lat ~ long | cut(mag, 3),
#     data = quakes,
#     groups =  cut(mag, 3),
#     aspect = "iso",
#     as.table = TRUE,
#     auto.key =
#       list(text = c("L", "M", "H"),
#            space = "top"),
#     type = c("p", "g"))

## ----quakesPlot5,ref.label='quakesPlotCode5',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long | cut(mag, 3), 
   data = quakes,
   groups =  cut(mag, 3),
   aspect = "iso",
   as.table = TRUE,
   auto.key = 
     list(text = c("L", "M", "H"),
          space = "top"),
   type = c("p", "g"))

## ----plotNMRCode1,echo=TRUE,eval=FALSE-----------------------------------
#  xyplot(I ~ ppm | Origin,
#    groups = Sample,
#    data = NMR.df,
#    type = "l",
#    layout = c(1,4),
#    as.table = TRUE)

## ----plotNMRCode0,echo=TRUE,eval=FALSE-----------------------------------
#  xyplot(I ~ ppm | Origin,
#    groups = Sample,
#    data = NMR.df,
#    type = "l",
#    layout = c(1,4),
#    as.table = TRUE,
#    xlim =
#      rev(extendrange(NMR.df$ppm)))

## ----bankingExample1,echo=TRUE-------------------------------------------
myplot <- 
  xyplot(sunspot.year ~
           1700:1988, 
         xlab = "", 
         type = "l")

## ----bankingExample2,echo=TRUE-------------------------------------------
myplot2 <- 
  update(myplot, 
         aspect = "xy")

## ----bankingExamplePlot1,echo=FALSE,fig.height=4,out.width='\\textwidth'----
myplot

## ----bankingExamplePlot2,echo=FALSE,fig.height=2,out.width='\\textwidth'----
myplot2

## ----quakesPlotCode0-a,ref.label='quakesPlotCode0',echo=TRUE,eval=FALSE----
#  xyplot(lat ~ long,
#     data = quakes,
#     aspect = "iso")

## ----quakesPlot0-a,ref.label='quakesPlotCode0',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long, 
   data = quakes,
   aspect = "iso")

## ----quakesPlotCode0-b,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.xyplot(...)
#      })

## ----quakesPlot0-b,ref.label='quakesPlotCode0-b',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.xyplot(...)
    })

## ----quakesPlotCode0-c,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.xyplot(...)
#      panel.grid()
#    })

## ----quakesPlot0-c,ref.label='quakesPlotCode0-c',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.xyplot(...)
    panel.grid()
  })

## ----quakesPlotCode0-d,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.grid()
#  
#      panel.xyplot(...)
#    })

## ----quakesPlot0-d,ref.label='quakesPlotCode0-d',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.grid()

    panel.xyplot(...)
  })

## ----quakesPlotCode0-e,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.grid(h=-1, v=-1)
#  
#      panel.xyplot(...)
#    })

## ----quakesPlot0-e,ref.label='quakesPlotCode0-e',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.grid(h=-1, v=-1)
    
    panel.xyplot(...)
  })

## ----quakesPlotCode0-f,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.grid(h=-1, v=-1)
#      panel.points(170, -30,
#         cex = 2, col = "purple",
#         pch = 19)
#  
#      panel.xyplot(...)
#    })

## ----quakesPlot0-f,ref.label='quakesPlotCode0-f',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.grid(h=-1, v=-1)
    panel.points(170, -30, 
       cex = 2, col = "purple", 
       pch = 19)
    
    panel.xyplot(...)
  })

## ----quakesPlotCode0-g,echo=TRUE,eval=FALSE------------------------------
#  xyplot(lat ~ long,
#    data = quakes,
#    aspect = "iso",
#    panel = function(...) {
#      panel.grid(h=-1, v=-1)
#      panel.points(170, -30,
#         cex = 2, col = "purple",
#         pch = 19)
#      panel.text(170, -30,
#         "Hi there!",
#         pos = 3)
#  
#      panel.xyplot(...)
#    })

## ----quakesPlot0-g,ref.label='quakesPlotCode0-g',echo=FALSE,fig.height=5.5,fig.width=5----
xyplot(lat ~ long,
  data = quakes,
  aspect = "iso",
  panel = function(...) {
    panel.grid(h=-1, v=-1)
    panel.points(170, -30, 
       cex = 2, col = "purple", 
       pch = 19)
    panel.text(170, -30,
       "Hi there!",
       pos = 3)
    
    panel.xyplot(...)
  })

## ----carsExample2,echo=TRUE,fig.width=6, fig.height=5,out.width='5cm'----
(pcars <- 
   xyplot(Price ~ EngineSize | 
     AirBags + Cylinders, 
     data = Cars93,
     subset = Cylinders %in% 
       c(4,6)))

## ----carsExample3,echo=TRUE,fig.width=6, fig.height=5,out.width='5cm'----
(pcars2 <- 
  useOuterStrips(pcars))

## ----carsExample4,echo=TRUE, fig.width=6,fig.height=5,out.width='5cm'----
(pcars3 <- 
   update(pcars2, 
          scale = "free"))

## ----carsExample5,echo=TRUE, fig.width=6,fig.height=5,out.width='5cm'----
combineLimits(pcars3)

## ----volcanoPlot,echo=FALSE,fig.width=5, fig.height=4--------------------
data(volcano)
panel.3d.contour <- function(x, y, z, rot.mat, distance, nlevels = 20,
                             zlim.scaled, ...) { 
  add.line <- trellis.par.get("add.line")
  panel.3dwire(x, y, z, rot.mat, distance, zlim.scaled = zlim.scaled,
               ...) 
  clines <- contourLines(x, y, matrix(z, nrow = length(x), byrow = TRUE), 
                         nlevels = nlevels) 
  for (ll in clines) {
    m <- ltransform3dto3d(rbind(ll$x, ll$y, zlim.scaled[2]), rot.mat, distance) 
    panel.lines(m[1,], m[2,], col = add.line$col, 
                lty = add.line$lty, lwd = add.line$lwd)
  } 
}
wireframe(volcano, zlim = c(90, 250), nlevels = 10,
          aspect = c(61/87, .3), panel.aspect = 0.6, 
          panel.3d.wireframe = "panel.3d.contour", 
          shade = TRUE, screen = list(z = 20, x = -60))

