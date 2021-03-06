## >> In R-studio, do Edit -> Folding -> Collapse all  (or Alt-O)
## works because four or more #, #### at end of comment headers 

#### Script for R user meeting, June 14th, 2017 ####
## "Basic data exploration, a graph with two y-axes, loops and functions"
## 
## Luc Steinbuch, Wageningen UR

## For this script I borrowed from:
## R Cookbook, Paul Teetor
## browseURL("http://shop.oreilly.com/product/9780596809164.do")
## and
## Foundations and Applications of Statistics, Randall Pruim
## browseURL("http://www.ams.org/publications/authors/books/postpub/amstext-13")
## and many other sources

#### Load a build-in dataset with continuous data ####

data(ToothGrowth)
?ToothGrowth

## Note: If you want to know more about build-in datasets: "data()"

## By the way: to import data from simple text files, 
## see https://www.r-bloggers.com/importing-data-to-r/
## Note that there are also packages to load for example spatial
## data, or data directly from databases, from excel etc etc.

## Personal preference: I often indicate what type 
## of variable I am dealing with
df_tg <- ToothGrowth
## df: dataframe; tg: toothgrowth

## remove obsolete variable
remove(ToothGrowth)

#### Summary, explore object  ####

class(df_tg) 
head(df_tg)
View(df_tg)
summary(df_tg)
nrow(df_tg)  # number of rows, often: number of observations


#### Subsetting a dataframe ####
## Means: selecting a part of..
## select column "len" (length) from dataframe
df_tg$len
## or equivently
df_tg[,1]  # "all rows, first column" 

## Select one element of a column 
df_tg$len[3]

## or equivently
df_tg[3, 1]  # "third row, first column"
df_tg[3, "len"]

## select a range
df_tg[5:15, 1] 

## note the vector generated by 
5:15

## select everything _except_ a range
df_tg[-(6:60), 1] 

## Often done: use a boolean vector (TRUE/FALSE variables)

vb_dose_is_05 <- df_tg$dose==0.5
## vb_: vector of booleans 

vb_dose_is_05

df_tg[vb_dose_is_05, ] #  select all rows with dose = 0.5, all columns

remove(vb_dose_is_05)

#### Explore data with an histogram ####
hist(df_tg$len)
## the bin width influences the appearance
hist(df_tg$len, breaks=20)
hist(df_tg$len, breaks=30)

## personally, I prefere the smoothened version
plot(
  x    = density(df_tg$len),
  main = "Effect of Vitamin C on Tooth Growth in Guinea Pigs - Density",
  xlab = "Tooth Growth"
)

## Note: the build-in function "density" retuns a "density" object;
## the "plot" function knows how to deal with such an object
class(density(df_tg$len))
density(df_tg$len)

## Note also:
## My personal preference is to write commands and function 
## arguments over several lines for clarity, rather than 
## everything in one line


## A bit more complicated: let's, for comparison, 
## plot the densities for the different doses
## separatly


## remember how to subset the "len" column
df_tg[df_tg$dose==0.5, "len"]


## first, plot density of "len" for one dose
plot(
  x    = density( df_tg[df_tg$dose==0.5, "len"] ),
  xlim = c(0,40), # now I have to set the axis manually
  ylim = c(0,0.12),  
  main = "",
  xlab = "Tooth Growth"
)

## then, plot the "len" for other dose, in another color 
## than the default black

lines(
  x   = density( df_tg[df_tg$dose==1, "len"] ),
  col = "red"
)

lines(
  x   = density( df_tg[df_tg$dose==2, "len"] ),
  col = "blue"
)

## always nice to add a legend :-)
legend(
  x      = "topright",
  legend = c(0.5, 1, 2),
  col    = c("black", "red", "blue"),
  lty    = "solid", # line type
  title  = "dose [mg/day]"
)



#### Boxplot ####
boxplot(
  df_tg[df_tg$dose==0.5, "len"],
  df_tg[df_tg$dose==1, "len"],
  df_tg[df_tg$dose==2, "len"],
  col   = c("grey", "red", "blue"),
  names = c(0.5, 1, 2)
)

remove(df_tg) # to keep a clean workspace
#### Bit more advanced: plot with two y-axis ####
## also: log scale in plot; grid lines; 
## complicated characters in plot text; 
## transparant colors

## load new dataset
?pressure
data(pressure)
df_pressure <- pressure; remove(pressure)
head(df_pressure)

par(mar = c(5,5,2,5))
## because the righ y-axis needs extra space, 
## we have to increase the standard right plot margin
## order: bottom, left, top, right

plot(
      x    = df_pressure$temperature,
      y    = df_pressure$pressure,
      type = "b", # plot type: line and dots combined     
      main = "Mercury pressure vs. temperature",
      xlab = expression(~degree~C),
      ylab = "pressure [mm]"
)

## Note "expression", this is a way to get non-latin symbols
## into your plot. See for example 
## http://statisticsr.blogspot.nl/2008/01/special-symbols-on-r-plot.html

grid( col = rgb(0,0,0, alpha=0.1 ) )
## In this case, I define the color black (red, green and blue
## all 0) with a high transparacy ('alpha channel')

par(new=T)
## Allow the nex plot to be plotted over the current plot
## with new settings

plot(
      x = df_pressure$temperature,
      y = df_pressure$pressure,
      type = "b",
      col  = "red",
      axes = FALSE, 
      # because we will add the right y-axis manually
      xlab = "",
      ylab = "",
      log  = "y" # plot y logaritmically
)

axis(side = 4, 
     col  = 'red',
     col.axis = "red"
     )

mtext(text = 'pressure [mm] log scale',
      side = 4, 
      line = 3, 
      col  = "red"
      )

grid( col = rgb(1,0,0, alpha=0.1) )

remove(df_pressure)

#### Loops and vectorized functions ####

## I borrowed from 
## https://www.datacamp.com/community/tutorials/tutorial-on-loops-in-r

## Let's start easy
vn_iteration_number <- seq(from=1, to=31, by=3)
## vn_: vector of numbers 

vn_iteration_number

for(n_iteration in vn_iteration_number) 
{
  n_squared <- n_iteration^2
  cat("\n", n_iteration, " ", n_squared)
}

## "cat" is a sophisticed version of "print"
## the string "\n" means: new line


## adding the results of a loop to a vector

vn_squared <- vector(mode="numeric", length=0)
## first define an empty vector

for(n_iteration in vn_iteration_number) 
{
  n_squared <- n_iteration^2
  cat("\n", n_iteration, " ", n_squared)
  vn_squared <- c(vn_squared, n_squared)
}

print(vn_squared)

#other way to do the same

vn_squared <- vn_iteration_number^2

vn_squared <- sapply( X   = vn_iteration_number, 
                      FUN = function(x) {x^2}
                     )
              
## In R, the 'apply' family of functions is an extremely 
## powerfull tool. See for example
## https://nsaunders.wordpress.com/2010/08/20/a-brief-introduction-to-apply-in-r/
remove(vn_squared,vn_iteration_number, n_squared, n_iteration)

## An example on demand: get the nummbers out of a 
## description textstring

## First, create a empty dataframe

df_farms <- as.data.frame(
    matrix(NA, ncol=3, nrow=16)
  )

## Assign column names
names(df_farms) <- c("farm_name", "farm_type", "farm_type_descr" )

## Fill the first column with strings
df_farms$farm_name <- c("1a", "1b", "1c", "1d", 
                        "2a", "2b", "2c", "2d",
                        "3a", "3b", "3c", "3d",
                        "4a", "4b", "4c", "4d")

df_farms

## for test: i=1

for(i in 1:16)
{
    ## To see what's going on
  cat("\n i:",i, " farm name: ", df_farms$farm_name[i])
  
  df_farms$farm_type[i] <-
    as.numeric(
        gsub("[^0-9]", "", df_farms$farm_name[i]) 
          )
  
}

## "gsub" is a find-and-replace function that uses 
## a 'regular expression': see
## https://en.wikipedia.org/wiki/Regular_expression
## In this case, it replaces everything that is not
## a number with a string of zero length
##
## Because the result of "gsub" is still a text string,
## it has to be converted to a number by "as.numeric"

df_farms


## Another way of doing the same:
## Let's write a function!

return_num <- function(x)
{
    n_farm_type  <-
        as.numeric(
            gsub("[^0-9]", "", x) 
              )

    return(n_farm_type)
}

## This self-written function with name 
## "return_num" takes an argument, 'x'
## and returs the result, 'n_farm_type'

## Test
return_num(x = "Co45v78fe5fe")

## Does this function also accepts vectors?
return_num(x = df_farms$farm_name)

## So, another way for above loop is
df_farms$farm_type <- return_num(df_farms$farm_name)

## Also possible
df_farms$farm_type <- sapply(X   = df_farms$farm_name, 
                             FUN = return_num
                    )


## if-then control structure: 
## let's write another function
## to do:  if this, than that..

return_descr <- function(x)
{
    if(x == 1) {
      str_descr <- "Farm type one"
      # another command
      # and yet another command
      }
    else if(x == 2)
      str_descr <- "Farm type two"
    else if(x == 3)
      str_descr <- "Farm type three"
    else if(x == 4)
      str_descr <- "Farm type four"
    else 
      str_descr <- "Undefined"
    
  return(str_descr)
}

## test
return_descr(0)
return_descr(3)

## Does it work with a vector as argument?
return_descr(df_farms$farm_type)
##Does not work; this function cannot handle vector inputs

## Solution 1: a "for" loop

for(i in 1:16)
{
  cat("\n i:",i, " farm type (num): ", df_farms$farm_type[i])
  df_farms$farm_type_descr[i] <- return_descr(df_farms$farm_type[i]) 
}

df_farms

## Solution 2: with "sapply"

df_farms$farm_type_descr <- sapply(X = df_farms$farm_type, 
                              FUN = return_descr
                            )

## Solution 3: use vector-enabled "ifelse" instead of "if" and "else"

df_farms$farm_type_descr <- 
    ifelse(test = df_farms$farm_type==1,
           yes  = "Farm type one",
           no   = "")

df_farms    

df_farms$farm_type_descr <- 
    ifelse(test = df_farms$farm_type==2,
           yes  = "Farm type two",
           no   = df_farms$farm_type_descr
           )

df_farms    

## etc etc.

remove(df_farms, i, return_descr, return_num)

#### Histogram for discrete data ####

## Again another dataset
?discoveries
data(discoveries)

class(discoveries)
## ts: timeseries object
## we make it a vector of numbers
vn_discoveries <- as.vector(discoveries)
vn_discoveries
remove(discoveries)
summary(vn_discoveries)

table(vn_discoveries)

hist(vn_discoveries)  
## Not what we want

plot(
  x    = table(vn_discoveries),
  type = "h", # horizontal bar
  xlab = "number of discoveries",
  ylab = "number of years"
)









