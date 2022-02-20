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
           #### DATOS ####
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
           #### Visualización ####
             tabPanel("Visualización",
                      tabsetPanel(
                        #### Consumo ####
                        tabPanel("Gráficas de consumo",
                                    div(
                                      br(),br(),
                                      p("A continuación podemos ver distintos gráficos que muestran el comportamiento de consumo energético
                                        del cliente.")
                                    ),
                                 hr(),
                                 
                                 fluidRow(
                                   h2("Consumo energético diario por minuto"), 
                                   br(),
                                   column(12,
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
                                          ),
                                          mainPanel(
                                            plotlyOutput("ConsumoSemanal"  ,height = "450px" 
                                                         
                                            )
                                          )
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   h2("Consumo energético diario"), 
                                   br(),
                                   column(12,
                                          sidebarPanel(
                                            # Selección del submétering
                                            selectInput(inputId = "Submetering", label = strong("Unidad de energía"),
                                                        choices = c("Sub_metering_1"   , "Sub_metering_2"     , "Sub_metering_3"  , "Global_active_power", "energia2" ),
                                                        selected = "Global_active_power"),
                                            # Selección del rango que se va a plottear
                                            dateRangeInput("dateRange", strong("Rango de fechas"), start = "2007-07-28", end = "2007-08-28",
                                                           min = "2006-12-16", max = "2010-11-26")
                                            ),
                                          mainPanel(
                                             plotOutput(outputId="ConsumoDiarioRango"  ,height = "450px" )
                                          )
                                     )
                                 ),
                                 hr(),
                                 fluidRow(
                                   h2("Consumo energético anual"), 
                                   br(),
                                   column(12,
                                   sidebarPanel(
                                     h4("Seleccione el año que quiere mostrar:"),
                                     radioButtons(inputId = "AnoPorcentaje", label = "",choices = 2007:2010,selected = 2009)
                                   ),
                              
                                 mainPanel(
                                   plotlyOutput("ConsumoMensual",height = "450px")
                                 )
                                 ) 
                                )  ,
                                
                                hr(),
                                fluidRow(
                                  h2("Evolución completa del consumo energético mensual"),
                                  br(),
                                  column(12,
                                         plotlyOutput("Prueba2",height = "500px")
                                  )
                                ),
                                 hr(),
                              #   mainPanel(
                              #     plotlyOutput("ConsumoSemanalMedioGlobal",height = "450px")
                              #   ),
                              #    hr(),
                              fluidRow(
                                h2("Comparación del consumo energético semanal medio"),
                                br(),
                                column(12,
                                 
                                       mainPanel(
                                  plotlyOutput("ConsumoMedio",height = "450px")
                                ),
                               
                                sidebarPanel(    
                                  helpText("Nota: Las barras corresponden al consumo energético semanal medio de los años 
                                       2006-2008, mientras que la línea corresponde al consumo energético semanal del año 2009.")),
                                p("Observamos como el submedidor que ha registrado un mayor consumo energético es el que corresponde 
                                  al aire acondicionado y termo eléctrico."),
                                p("Además, para los tres submedidores podemos ver que el consumo en el año 2009 ha sido superior a 
                                  la media de los últimos años, es decir, en el último año el consumo energético se ha visto incrementado, especialmente se ha registrado un mayor aumento del consumo energético del termo y aire acondicionado.")
                             
                                ),
                              ),
                              hr(),
                              
                              fluidRow(
                                   h2("Consumo energético semanal"), 
                                   br(),
                                   column(12,
                                   sidebarPanel(
                                     h4("Seleccione la semana del año que quiere mostrar:"),
                                     # price slider
                                     sliderInput(inputId = "Semana", 
                                                 label = "", 
                                                 min = 1, max = 52, 
                                                 value = 1),
                                     br(),
                                     helpText("Nota: Las barras corresponden al consumo energético semanal medio de los años 
                                       2006-2008, mientras que la línea corresponde al consumo energético semanal del año 2009.")
                                   ),
                                 mainPanel(
                                   plotlyOutput("ConsumoSemana",height = "450px")
                                 ) ,
                             
                                 br(),hr(),br(),
                                 includeHTML("html/footer.html")
                                 )
                              )
                                   ),
                        #### Porcentaje Energia ####
                        tabPanel("Porcentaje de energía",
                                 fluidRow(
                                    h2("Porcentaje total del consumo energético"),
                                     br(),
                                     column(12,
                                  sidebarPanel(
                                    h4("Seleccione una fecha"),
                                     numericInput("DiaPieAnual", "Día del mes", value = 1, min = 1, max = 31),
                                     numericInput("MesPieAnual", "Mes", value = 1, min = 1, max = 12),
                                     numericInput("AnoPieAnual", "Año", value = 2009, min = 2006, max = 2010) 
                                     ),
                                 mainPanel(
                                   plotlyOutput("PieAnual",height = "450px")
                                   )
                                 )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(12,
                                          sidebarPanel(
                                            dateInput("DatePie", strong("Fecha"), value =  "2009-01-02", 
                                                      min = "2006-12-16", max = "2010-11-26")
                                          ) ,
                                          mainPanel(
                                            h3("Porcentaje del consumo energético según la hora del día"),
                                            br(),
                                            plotlyOutput("Prueba",height = "450px",width = "80%")
                                          )
                                          )
                                   
                                 ),
                                 
                                 br(),hr(),br(),
                                 includeHTML("html/footer.html")
                                 
                              )
                        
                            )
                        ),
           #### Series Temporales ####
           navbarMenu(
            
             "Pronóstico de consumo",
             tabPanel("Diaria"),
             tabPanel("Mensual",
                      tabsetPanel(
                        tabPanel(
                          "Cocina",
                          br(),
                          h1("Evolución mensual del consumo. Años 2007-2010 y predicción para el año 2010"),
                          br(),
                          fluidRow(
                            h2("Visualización de los datos"),
                            column(9,
                                 img(src = "PlotMensualCocina.png" , width="100%")
                            ),
                            column(3,
                                   div(
                                     br(),
                                     p("
                                       Podemos apreciar como el consumo energético durante los meses de verano es más bajo"),
                                     p("Además, la línea discontinua roja muestra la tendencia de consumo energético, que en este caso es creciente a medida que avanza el tiempo."),
                                     p("En definitiva, la gráfica nos muestra el comportamiento de consumo energético de la cocina. Podemos observar dos picos donde el consumo ha sido
                                       bastante inferior al resto, en el mes de Junio del año 2008 y el mes de Mayo del año 2009. \n Estos dos valores son valores atípicos,
                                       son unos valores aislados que podrían influir en el pronóstico del consumo."),
                                     style="text-align:justify; margin: auto;"
                                     )  
                            )
                          ),
                          hr(),
                          fluidRow(
                            h2("Descomposición de la serie"),
                            column(7,
                                   img(src = "PlotMensualDescomposicionCocina.png" , width="100%")),
                            column(5,
                                   div(
                                     br(),
                                     p("Data: Es el mismo gráfico que tenemos en el apartado visualización"),
                                     p("Remainder: Ruido. Corresponde a las variaciones irregulares que no se pueden predecir"),
                                     p("Seasonal: Componente estacional: Podemos ver que hay movimientos que se repiten cada año, es decir,
                                       que el consumo energético mensual es similar para los mismos meses en distintos años."),
                                     p("Trend: La tendencia es el conportamiento a largo plazo del consumo. En nuestro caso, la tendencia del consumo crece con los años."),
                                     style="text-align:justify; margin: auto;"
                                   ))
                          ),
                          hr(),
                          fluidRow(
                            h2("Visualización del consumo sin estacionalidad"),
                            column(12,
                                   h3("Serie sin estacionalidad"),
                                   img(src = "PlotMCocinaSinEstac.png" , width="100%", height=400)
                                   )
                            
                          ),
                          hr(),
                          fluidRow( 
                            h2("Pronóstico del consumo energético para el año 2010"),
                            br(),
                            column(5,
                                   img(src = "PredConsMenCoc.png" , width="100%")
                                   ),
                            column(5,
                                   img(src = "PredConsMensCoc2.png" , width="100%")
                                   ),
                            column(2,
                                  div(
                                   p("Observamos como el pronóstico del consumo energético
                                     para el período comprendido entre los meses Mayo-Junio es bastante inferior al resto
                                     en ambos casos."),
                                   p("El pronóstico de consumo es bastante superior en 
                                     el caso que no eliminamos la estacionalidad."),
                                   style="text-align:justify; margin: auto;")
                                   )
                          ),
                        
                          br(),hr(),br(),
                          includeHTML("html/footer.html")
                        ),
                        tabPanel(
                          "Lavadero"
                        ),
                        tabPanel(
                          "Termo eléctrico y aire acondicionado"
                        )
                      ),
                      
                      
             ),
             tabPanel("Semanal")
             )
             
           
))


  


