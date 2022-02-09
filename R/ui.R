library(shiny)
library(shinythemes)
library(plotly)
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


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  #### NavBarPane ####
  navbarPage( "IOT Analitycs", 
             selected = icon("home"), collapsible = TRUE, fluid = TRUE, 
           theme = shinytheme("simplex"),
           tabPanel( icon("home"),
                       includeHTML("html/Home.Rhtml"),
                     includeHTML("html/footer.html")
             ),
           navbarMenu(
             "Datos",
                      tabPanel("Descripción de los datos",
                              includeHTML("html/DatosDescripcion.Rhtml"),
                              includeHTML("html/footer.html")
                      ),
                      tabPanel("Granularidad meses",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de meses "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Meses"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad mensual"),style="text-align:center; font-family: times"),
                               includeHTML("html/footer.html")
                      ),
                      tabPanel("Granularidad diaria",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad diaria"),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Dias"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad diaria"),style="text-align:center; font-family: times"),
                               includeHTML("html/footer.html")
                      ),
                      tabPanel("Granularidad horas",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de horas "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Horas"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad de horas"),style="text-align:center; font-family: times"),
                               includeHTML("html/footer.html")
                      ),
                      
                      tabPanel("Granularidad minutos",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de minutos "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Minutos"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad de minutos"),style="text-align:center; font-family: times"),
                               includeHTML("html/footer.html")
                      )
                      ),
             tabPanel("Visualización",
                      tabsetPanel(
                        tabPanel("Gráficas de consumo",
                                    div(
                                      p(),p(),p()
                                    ),
                                   div(
                                     sidebarPanel(
                                     h4("Seleccione una fecha"),
                                     br(),
                                     numericInput("Dia", "Día del mes", value = 1, min = 1, max = 31),
                                     numericInput("Mes", "Mes", value = 1, min = 1, max = 12),
                                     numericInput("Ano", "Año", value = 2009, min = 2006, max = 2010)
                                     # ,
                                    ##  br(),
                                    ##  # output 1
                                    ##  h5("Fecha Seleccionada")),
                                    ##  h2(strong(textOutput("Dia")," de ",textOutput("Mes"), "del año", textOutput("Ano")," ."))
                                      )
                                   ),
                                   mainPanel(
                                     plotlyOutput("ConsumoSemanal",height = "450px")
                                   ),
                                 hr(),
                                 div(
                                   sidebarPanel(
                                     p("Seleccione el año que quiere mostrar:"),
                                     radioButtons(inputId = "AnoPorcentaje", label = "",choices = 2007:2010,selected = 2009)
                                   )
                                 ),
                                 mainPanel(
                                   plotlyOutput("ConsumoMensual",height = "450px")
                                 ),
                                 hr(),
                
                              #   mainPanel(
                              #     plotlyOutput("ConsumoSemanalMedioGlobal",height = "450px")
                              #   ),
                              #    hr(),
                                 div(
                                   sidebarPanel(
                                     p("Seleccione la semana del año que quiere mostrar:"),
                                     numericInput("Semana", label = "Semana",value = 1, min = 1, max = 52)
                                   )
                                 ),
                                 mainPanel(
                                   plotlyOutput("ConsumoSemana",height = "450px")
                                 )
                                 
                                   ),
                        tabPanel("Porcentaje de energía",
                                   div(br()),
                                
                                   div(
                                 div(
                                   sidebarPanel(
                                     h4("Seleccione una fecha"),
                                     br(),
                                     numericInput("DiaPieAnual", "Día del mes", value = 1, min = 1, max = 31),
                                     numericInput("MesPieAnual", "Mes", value = 1, min = 1, max = 12),
                                     numericInput("AnoPieAnual", "Año", value = 2009, min = 2006, max = 2010)
                                     
                                   )
                                 ),
                                 mainPanel(
                                   plotlyOutput("PieAnual",height = "450px")
                                 )
                                   ),
                                 div(
                                   div(
                                     sidebarPanel(
                                       h4("Seleccione una fecha"),
                                       br(),
                                       numericInput("DiaPieAnualPorHora", "Día del mes", value = 1, min = 1, max = 31),
                                       numericInput("MesPieAnualPorHora", "Mes", value = 1, min = 1, max = 12),
                                       numericInput("AnoPieAnualPorHora", "Año", value = 2009, min = 2006, max = 2010)
                                       
                                     )
                                   ),
                                   mainPanel(
                                    h3("Porcentaje del consumo energético según la hora del día"),br(),
                                    plotlyOutput("Prueba")
                                   )
                                 )

                                 )
                              )
                            )
                        )
)



  

