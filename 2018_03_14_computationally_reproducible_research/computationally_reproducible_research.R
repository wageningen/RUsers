
sessionInfo()

library(knitr)

## run a .Rmd file in the Global environment
knit('RMarkdown_example_1.Rmd')
# don't forget to remove RMarkdown_example_1.md, and figure,
# afterwards

## Extract R code into .R file from .Rmd file
purl('RMarkdown_example_1.Rmd')
## and run it
source('RMarkdown_example_1.R')


## Create report from .R document with RMarkdown code 
## behind 'roxygen' comments 
library(rmarkdown)
render(input = "RMarkdown_example_2.R",
       output_format = "html_document", # other options: https://rmarkdown.rstudio.com/lesson-9.html
       clean = TRUE  # removes temporarily files
       )

browseURL('RMarkdown_example_2.html')
