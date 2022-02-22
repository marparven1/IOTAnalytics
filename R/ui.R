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
load(file="data/Pronostico/DF_Energia_PronosticoMensual_01.RData")
load(file="data/Pronostico/DF_Energia_PronosticoAgosto_11.RData")



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
                  tabPanel("Descripción de los datos",
                              includeHTML("html/DatosDescripcion.Rhtml"),
                              includeHTML("html/footer.html")
                      ),
           #### Visualización ####
           navbarMenu(
             "Visualización",
             #### Consumo ####
                        tabPanel("Gráficas personalizables",
                                 fluidRow(
                                   h2("Evolución completa del consumo energético mensual"),
                                   br(),
                                   column(12,
                                          plotlyOutput("Prueba2",height = "500px")
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
                                                        choices = c("Cocina"="Sub_metering_1"   ,"Lavadero"= "Sub_metering_2"     ,"Aire y termo eléctrico"= "Sub_metering_3"  ,"Energía global"= "Global_active_power", "Resto de la casa"="energia2" ),
                                                    
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
                                            plotlyOutput("ConsumoSemanal"  ,height = "450px")
                                          )
                                   ),
                                 ),
                                 div( br(),
                                      includeHTML("html/footer.html"))
                                 ),
                        tabPanel("Gráficas generales",
                                 fluidRow(
                                   h2("Comparación del consumo energético semanal medio"),
                                   br(),
                                   column(12,
                                          
                                          mainPanel(
                                            plotlyOutput("ConsumoMedio",height = "450px")
                                          ),
                                          sidebarPanel(    
                                            helpText("*Nota*: Las barras corresponden al consumo energético diario medio de los distintos días de la semana para los años 2006-2008, mientras que la línea corresponde al consumo energético diario medio de los distintos días de la semana para el año 2009.")),
                                       p("Observamos como el submedidor que ha registrado un mayor consumo energético es el que corresponde 
                                  al aire acondicionado y termo eléctrico."),
                                  p("Además, para los tres submedidores podemos ver que el consumo en el año 2009 ha sido superior a 
                                  la media de los últimos años, es decir, en el último año el consumo energético se ha visto incrementado, especialmente se ha registrado un mayor aumento del consumo energético del termo y aire acondicionado.")
                                  
                                   ),
                                 ),
                                 div( br(),
                                      includeHTML("html/footer.html"))
                                ),
             #### Porcentaje Energia ####
                        tabPanel("Porcentaje de energía",
                                 tabsetPanel(
                                   tabPanel("Gráficos generales",
                                            
                                          fluidRow(
                                            br(),
                                            h2("Porcentaje total del consumo energético"),
                                            br(),
                                            column(12,
                                                   mainPanel(
                                                     plotlyOutput("PieTotal",height = "450px")
                                                   ))
                                          )  ,
                                          hr(),
                                          fluidRow(
                                            br(),
                                            h2("Comparación del porcentaje de consumo energético en verano e invierno"),
                                            h3("Porcentaje del total de consumo energético en los meses de  Enero y Febrero"),
                                            br(),
                                            column(6,
                                                
                                                     plotlyOutput("PieInvierno1",height = "450px",width = "100%")) ,
                                            column(6,   
                                                 
                                                     plotlyOutput("PieInvierno2",height = "450px",width = "100%"))
                                            ),
                                          hr(),
                                          fluidRow(
                                            br(),
                                            h3("Porcentaje del total de consumo energético en los meses de  Julio y Agosto"),
                                            br(),
                                            column(6,
                                                  
                                                     plotlyOutput("PieVerano1",height = "450px",width = "100%")  ),
                                            column(6,   
                                                   
                                                     plotlyOutput("PieVerano2",height = "450px",width = "100%"))
                                            ),
                                          div(
                                            h3("Conclusiones:"),
                                            p("Podemos observar que el porcentaje de consumo energético del termo
                                              eléctrico y del aire acondicionado ha sido superior en los meses de
                                              verano para todos los años salvo para el año 2010, donde el consumo
                                              fue mayor durante los meses de invierno."),
                                            p("Con respecto al lavadero, el consumo energético en verano también fue superior al de invierno."),
                                            p("El consumo energético que ha registrado el submedidor de la cocina
                                              ha tenido el mismo comportamiento que el del aire y termo, es decir,
                                              el porcentaje de consumo fue superior en verano, salvo para el año 
                                              2010.")
                                          )
                                          ),
                                   tabPanel("Gráficos personalizables",
                                            fluidRow(
                                              h2("Porcentaje del consumo energético anual"),
                                              br(),
                                              column(12,
                                                     sidebarPanel(
                                                       h4("Seleccione el año que quiere mostrar:"),
                                                       radioButtons(inputId = "AnoPie", label = "",choices = 2006:2010,selected = 2009),
                                                      helpText("Nota: Los porcentajes de los años 2006 y 2010 no corresponden a 
                                                               años completos. \n Para el año 2006 el período comprendido es del 16 al 31 de Diciembre y para el
                                                               año 2010, desde el 1 de Enero al 31 de Diciembre.")
                                                     ),
                                                     mainPanel( plotlyOutput("PieUnAno",height = "450px"))
                                                     ),
                                              div(
                                                p("Seleccionando diferentes años podemos ver como el consumo 
                                                  energético varía cada año. Cabe destacar que desde el año 2007 al
                                                  año 209 el consumo energético correspondiente al termo eléctrico y 
                                                  aire acondicionado ha aumentado de un 31% a casi un 38% del consumo
                                                  total.")
                                              )
                                            ),
                                            fluidRow(
                                              h2("Porcentaje del consumo energético mensual"),
                                              br(),
                                              column(12,
                                                     sidebarPanel(
                                                       selectInput(inputId = "MesAno", label = strong("Mes del año"),
                                                                   choices = c("Enero"=1   ,
                                                                               "Febrero"= 2    ,
                                                                               "Marzo"= 3  ,
                                                                               "Abril"= 4, 
                                                                               "Mayo"=5,
                                                                               "Junio"=6,
                                                                               "Julio"=7,
                                                                               "Agosto"=8,
                                                                               "Septiembre"=9,
                                                                               "Octubre"=10,
                                                                               "Noviembre"=11,
                                                                               "Diciembre"=12),
                                                                   
                                                                   selected = 1)
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput("PieMensual",height = "450px")
                                                     )
                                                     
                                                     )
                                              ),
                                            fluidRow(
                                              h2("Porcentaje del consumo energético mensual por años"),
                                              br(),
                                              column(12,
                                                     sidebarPanel(
                                                       selectInput(inputId = "MesAno2", label = strong("Mes del año"),
                                                                   choices = c("Enero"=1   ,
                                                                               "Febrero"= 2    ,
                                                                               "Marzo"= 3  ,
                                                                               "Abril"= 4, 
                                                                               "Mayo"=5,
                                                                               "Junio"=6,
                                                                               "Julio"=7,
                                                                               "Agosto"=8,
                                                                               "Septiembre"=9,
                                                                               "Octubre"=10,
                                                                               "Noviembre"=11,
                                                                               "Diciembre"=12), selected = 1),
                                                       radioButtons(inputId = "AnoPie2", label = "",choices = 2006:2010,selected = 2009)
                                                       
                                                     ),
                                                     mainPanel(
                                                       plotlyOutput("PieMensualAno",height = "450px")
                                                     )
                                                     
                                              )
                                            ),
                                            fluidRow(
                                              h2("Porcentaje del consumo energético por día"),
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
                                              
                                            ))
                                 ),
                                 br(),hr(),br(),
                                 includeHTML("html/footer.html")
                                 
                                 )
                        ),
           
           #### Series Temporales ####
           navbarMenu(
            
             "Pronóstico de consumo",
             tabPanel("Diario",
                      h1("Pronóstico del consumo energético diario para el mes de Agosto del año 2011"),
                      fluidRow( 
                      
                        column(12,
                               plotlyOutput("PronosticoEnero",height = "450px",width = "80%")
                        ),
                               div(
                                 p("Se observa que el pronóstico de consumo para el mes de Diciembre es bastante superior al resto de meses"),
                                style="text-align:justify; margin: auto;")
                      ),
                      
                      br(),hr(),br(),
                      includeHTML("html/footer.html")
                      ),
             tabPanel("Mensual",
                          h1("Pronóstico del consumo energético mensual para el año 2011"),
                         
                          fluidRow( 
                            h2("Pronóstico del consumo energético"),
                            h3("Diciembre del año 2011 y año 2011"),
                            br(),
                            column(10,
                                   plotlyOutput("PronosticoMensual",height = "450px",width = "80%")
                                   ),
                            
                            column(4,
                                  div(
                                    p("Se observa que el pronóstico de consumo para el mes de Diciembre es bastante superior al resto de meses"),
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
             tabPanel("Semanal")
             )
             
)
)




  


