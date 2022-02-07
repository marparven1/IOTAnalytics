library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(shinyjs)
library(rintrojs)
library(markdown)

load(file="data/DF_Energia_GMinutos.RData")
load(file="data/DF_Energia_GHoras.RData")
load(file="data/DF_Energia_GDiaria.RData")
myfile <-  file.path("data", "DF_Energia_GMensual.RData") 
load(myfile)


source("R/ui.R")
source("R/server.R")



shinyApp(ui = ui,server=server)