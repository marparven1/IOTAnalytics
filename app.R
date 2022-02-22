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
load(file="data/Pronostico/DF_Energia_PronosticoMensual_01.RData")
load(file="data/Pronostico/DF_Energia_PronosticoAgosto_11.RData")


source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui,server=server)
