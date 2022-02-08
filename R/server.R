library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
library(plotly)
library(quantmod)




 
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #

  
  output$Meses<- DT::renderDataTable(
    DT::datatable({
      Granularidad_meses
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgb(236, 85, 107)', 'color': 'white'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = colnames(Granularidad_meses)
    ))
 


  
  output$Dias<- DT::renderDataTable(
    DT::datatable({
      Granularidad_dias
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgb(236, 85, 107)', 'color': 'white'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = colnames(Granularidad_dias)
    ))
  
  output$Horas<- DT::renderDataTable(
    DT::datatable({
      Granularidad_horas
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgb(236, 85, 107)', 'color': 'white'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = colnames(Granularidad_horas)
    ))
  
  output$Minutos<- DT::renderDataTable(
    DT::datatable({
      DatosCompletos
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'rgb(236, 85, 107)', 'color': 'white'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = colnames(DatosCompletos)
    ))
  
  
  
  ## Plot Consumo semanal 

  
  plotData <- reactive({
    DatosCompletos %>% 
      filter(year == input$Ano) %>% 
      filter(month == input$Mes) %>% 
      filter(day == input$Dia)
  })


  
  output$ConsumoSemanal <- renderPlotly({
    
    plot_line <- plotData() 
      plot_ly(plot_line,
              x = ~plot_line$DateTime,
              y = ~plot_line$Sub_metering_1,
              name = 'Cocina', type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~plot_line$Sub_metering_2, name = 'Lavandería', mode = 'lines') %>%
      add_trace(y = ~plot_line$Sub_metering_3, name = 'Termo eléctrico & AC', mode = 'lines') %>%
      layout(title = "Consumo energético de cada submedidor durante un día completo",
             xaxis = list(title = "Hora"),
             yaxis = list (title = "Energía (watt-hours)"))
    
  })
  
  
  ### Añadir la fecha seleccionada
#   trend_data <- reactive({
# 
#     
#     DatosCompletos %>% 
#       filter(year == input$Ano) %>% 
#       filter(month == input$Mes) %>% 
#       filter(day == input$Dia)
#     
#   })
#   
#   ## Dia
#   output$Dia <- renderText({
#     
#     overview <- trend_data()
#     scales::comma(length(unique(overview$Dia)))
#     
#   })
#   
#   ## Mes
#   output$Mes <- renderText({
#     
#     overview <- trend_data()
#     scales::comma(sum(overview$Mes))
#     
#   })
#   
#   ## Ano
#   output$Ano <- renderText({
#     
#     overview <- trend_data()
#     scales::comma(sum(overview$Ano))
#     
#   })
  
  
  
  PorcData <- reactive({
    Granularidad_meses %>% 
      filter(year == input$AnoPorcentaje) 
  })
  
  
  
  output$PorcentajeConsumo <- renderPlotly({
    
    plot_line <- PorcData() 
    
    plot_ly(x=~plot_line$`Date-MY`,
            y = ~plot_line$Sub_metering_1,
            type = 'scatter',mode = 'lines+markers',name="Cocina",
            line=list(color='rgb(199, 0, 57 )'),marker = list(color =   'rgb(199, 0, 57  )')
            ) %>%
      add_trace(y = ~plot_line$Sub_metering_2, name = 'Lavadero', 
                mode ='lines+markers',
                line=list(color='rgb(254, 237, 108)'),
                marker = list(color =   'rgb(243, 221, 63  )')) %>%
      add_trace(y = ~plot_line$Sub_metering_3, name = 'Calefacción & AC', mode = 'lines+markers',
                line=list(color='rgb(150, 218, 108)'),
                marker = list(color ='rgb(150, 218, 108)')) %>%
      layout(title = "Consumo energético por meses.",
             xaxis = list(title = "2007",
                          tickvals=list( "2007-01" ,"2007-02"  ,"2007-03" ,"2007-04" ,"2007-05"  ,"2007-06","2007-07" ,"2007-08"  ,"2007-09","2007-10" ,"2007-11"  ,"2007-12" )),
             yaxis = list (title = "Energía (Varios-hora)"))
    
    
  }) 
  
  

  
  
  
  
  
  
  
  
  
}