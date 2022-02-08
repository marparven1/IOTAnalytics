library(shiny)
library(shinythemes)




# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  #### NavBarPane ####
  navbarPage("IOT Analitycs", 
             selected = icon("home"), collapsible = TRUE, fluid = TRUE, 
           theme = shinytheme("simplex"),
             tabPanel( icon("home"),
                       includeHTML("Untitled.Rhtml"),
                       HTML('<footer><p><a href="https://iot-analytics.com/" target="_blank">IOT Analitycs</a></p></footer>')
             ),
           navbarMenu(
             "Datos",
                      tabPanel("Descripción de los datos",
                               
                               div(
                                 h1("Descripción de los datos"),
                                 p("Nuestros datos contienen información sobre mediciones de energía de una casa en Sceaux (7km of Paris, France) 
                                    en los meses de Diciembre de 2006 a Noviembre de 2010 (47 meses). Contiene un total de 2075259 medidas en 10 
                                    varaibles que explicaremos a continuación"),
                                 h2("Variables"),
                                 p( "- ",strong("                 Date:"), " Fecha en formato dd/mm/yyyy"),
                                 p( "- ",strong("                 Time:"), " Hora en formato hh:mm:ss."),
                                 p( "- ",strong("  global_active_power:"), " potencia activa media global por minuto en el hogar (kw). Es la potencia “útil”, la que consumen lls equípos eléctricos"),
                                 p( "- ",strong("global_reactive_power:"), "potencia reactiva media global por minuto en el hogar (kw)"),
                                 p( "- ",strong("              voltage:"), " Voltaje medio por minuto (voltios)"),
                                 p( "- ",strong("     global_intensity:"), " Intensidad media de corriente por minuto en el hogar (amperios) household global minute-averaged current intensity (in ampere) ") ,
                                 p( "- ",strong("       sub_metering_1:"), " energía del subetering 1 (Wh de energía activa). Corresponde a:"),
                                 p( "- ",strong("       sub_metering_2:"), " energía del subetering 2 (Wh de energía avtiva).Corresponde a:"),
                                 p( "- ",strong("       sub_metering_3:"), " energía del subetering No. 3 (Wh de energía activa).Corresponde a:"),
                                 p( "- ",strong("             DateTime:"), " Fecha de la medición, en formato Fecha-Hora(minutos:segundos)")
                               )
                      ),
                      tabPanel("Granularidad meses",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de meses "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Meses"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad mensual"),br("Marta Venegas P."),style="text-align:center; font-family: times")
                      ),
                      tabPanel("Granularidad diaria",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad diaria"),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Dias"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad diaria"),br("Marta Venegas P."),style="text-align:center; font-family: times")
                      ),
                      tabPanel("Granularidad horas",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de horas "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Horas"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad de horas"),br("Marta Venegas P."),style="text-align:center; font-family: times")
                      ),
                      
                      tabPanel("Granularidad minutos",
                               tags$style(".fa-database {color:#E87722}"),
                               h3(p(em("Dataset con granularidad de minutos "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
                               fluidRow(column(DT::dataTableOutput("Minutos"),
                                               width = 12)),
                               hr(),
                               p(em("Dataset con granularidad de minutos"),br("Marta Venegas P."),style="text-align:center; font-family: times")
                      )
                      ),
             tabPanel("Visualización",
                      tabsetPanel(
                        tabPanel("Gráficas 1",
                                 div(
                                   h1("Gráficas del consumo semanal"),
                                   div( 

                                     img( src="Consumo_SemanalMedioSemana52_Comparacion_09_06-08.png", width="50%", alt="Semanalmedio52"),
                                     br(),
                                     img( src="Consumo_SemanalMedio_Comp_09_06-08.png", width="50%", alt="Semanal medio")
                                     ),
                                 )
                                 ),
                        tabPanel("Porcentaje de energía"
                      )
                      
                      )
  )
))

  

