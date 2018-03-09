#' ---
#' title: "RMarkdown Minimal Example"
#' author: "Pie Bear"
#' date: "19 August 2016"
#' output: html_document
#' ---
#' 
#' ## Markdown 

#' This was an **RMarkdown** document, originating from  [here](http://www.geo.uzh.ch/microsite/reproducible_research/post/rr-r-publication/). You can write text in **bold** or _cursive_, include [links](http://geo.uzh.ch) or add inline formulae $y=x+3$ (following Latex formulae markup)
#'
#' Now it is a **.R** document with _roxygen2_ style comments combined with RMarkdown; more info at [https://rmarkdown.rstudio.com/articles_report_from_r_script.html](https://rmarkdown.rstudio.com/articles_report_from_r_script.html)

#' ## Markdown with R code
#+ chunk_name1

d  <- data.frame(participants=1:10,height=rnorm(10,sd=30,mean=170)) 
summary(d)

#' you can include R elements inline and generate plots:

#+ chunk_name2, fig.width=8, fig.height=4
plot(d)

#+ chunk_name3
#' There were `r nrow(d)` participants.