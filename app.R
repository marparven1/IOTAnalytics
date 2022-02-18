library(shiny)
library(dplyr)
library(tibble)
library(purrr)
library(ggplot2)
library(shinyjs)
library(rintrojs)
library(markdown)
library(DT)
library(tidyr)



load(file="data/DF_Energia_GMinutos.RData")
load(file="data/DF_Energia_GHoras.RData")
load(file="data/DF_Energia_GDiaria.RData")
load(file="data/DF_Energia_GMensual.RData")


source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui,server=server)
