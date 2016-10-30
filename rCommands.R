#Update all user installed R packages
install.packages( 
  lib  = lib <- .libPaths()[1],
  pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package,
  type = 'source'
)

library(curl)
download.file(url = "https://docs.google.com/spreadsheets/d/1NzknyOH5lrlcehOCpuwqJOAkMalgZvqFpsskRtg9EWY/pub?gid=1344723553&single=true&output=csv", destfile="data/PisaSelectIndex.csv", 'curl')

download.file(url = "https://docs.google.com/spreadsheets/d/1Odijhx6-VTfQvGux5pDDgqbWpRRRS8o3t5f_nV3pbb8/pub?gid=2021837635&single=true&output=csv", destfile="data/PisaSelectIndex.csv", 'curl')

https://docs.google.com/spreadsheets/d/1Odijhx6-VTfQvGux5pDDgqbWpRRRS8o3t5f_nV3pbb8/edit#gid=2021837635
PisaSelectIndex<-read.csv("data/PisaSelectIndex.csv", header = TRUE, sep=",")
https://docs.google.com/spreadsheets/d/1Odijhx6-VTfQvGux5pDDgqbWpRRRS8o3t5f_nV3pbb8/pub?gid=2021837635&single=true&output=csv


devtools::install_github("hadley/ggplot2")
shiny - library='/usr/local/lib/R/site-library' 
avnerkantor - library='/home/avnerkantor/R/x86_64-pc-linux-gnu-library/3.3'

devtools::install_github("ropensci/plotly")
shiny library='/usr/local/lib/R/site-library'
avnerkantor - library='/home/avnerkantor/R/x86_64-pc-linux-gnu-library/3.3' 