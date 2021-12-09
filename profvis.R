library(shiny)
library(profvis)

profvis::profvis(rmarkdown::run("COVID-19-Bulgaria-v2.Rmd"))
