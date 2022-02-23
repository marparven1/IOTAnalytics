library( shiny      )
library( shinythemes)
library( DT       )
library( ggplot2  )
library( nortest  )
library( tseries  )
library( RcmdrMisc)
library( lmtest   )
library( plotly   )
library( quantmod )
library( dplyr    )
library( tibble   )
library( purrr    )
library( shinyjs  )
library( rintrojs )
library( markdown )
library( tidyr    )





 
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #

#### DataTables ####
  
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
  
#### Gráficas ####
  
## Plot Consumo semanal por minuto ####

  
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
      layout(title = "Consumo energético de cada submedidor \n durante un día completo",
             xaxis = list(title = "Hora"),
             yaxis = list (title = "Energía por minuto (watt-hours)"))
    
  })
  
  
### Añadir la fecha seleccionada ####
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
  
##  Consumo energético diario ####  

  
 DatosRango<- Granularidad_dias%>%
   pivot_longer(cols = c(7,8,9,10,11),
                names_to = "Energia",
                values_to = "valorEnergetico") %>% 
   select(Date , Energia , valorEnergetico)
 
   
 fecha_seleccionada <- reactive({
   req(input$dateRange)
   validate(need(!is.na(input$dateRange[1]) & !is.na(input$dateRange[2]), "Error: Seleccione fecha de inicio y de fin."))
   validate(need(input$dateRange[1] < input$dateRange[2], "Error: La fecha de inicio no puede ser anterior a la fecha de fin."))
   DatosRango  %>%
     filter(
       Energia == input$Submetering,
       Date >  as.POSIXct( input$dateRange[1] )  & Date < as.POSIXct( input$dateRange[2] )
       
   )
 })
   ## ConsumoDiarioRango
   
    output$ConsumoDiarioRango <- renderPlot({
      plot(x =  fecha_seleccionada()$Date, 
           y =   fecha_seleccionada()$valorEnergetico,
           type = "b",
           xlab = "Fecha", 
           ylab = "Energía (Vatios-Hora)", 
           col = "red",
           main="Evolución del consumo energético",
           sub = "Se muestra para cada día la energía total consumida ese día"
           )
    }) 
   
  
# output$ConsumoDiarioRango <- renderPlotly({
#   
#   fecha <- fecha_seleccionada()
#   plot_ly(data = fecha,
#           x=~fecha$Date,
#           y = ~fecha$valorEnergetico,
#           type = 'scatter',
#           mode = 'lines',
#           line=list(color='rgb(199, 0, 57 )')
#   ) %>% 
#     layout(title = "Evolución del consumo energético anual",
#            xaxis = list(title = "Fecha"
#                        #  ,
#                        #  ticktext = list("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), 
#                        #  tickvals = fecha_seleccionada()$Date
#                         ),
#            yaxis = list (title = "Energía (Varios-hora)"))
# }) 
  
  
  
  
  
## Plot del consumo anual ####
  
PorcData <- reactive({
    Granularidad_meses %>% 
      filter(year == input$AnoPorcentaje) 
  })
  
  
  
  
output$ConsumoMensual <- renderPlotly({
  
  plot_line <- PorcData() 
  
  plot_ly(
    x=~plot_line$`Date-MY`,
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
    add_trace(y = ~plot_line$Global_active_power, name = 'Energía total', mode = 'lines+markers',
              line=list(color=     'rgb(89, 118, 236  )'),
              marker = list(color ='rgb(89, 118, 236  )')) %>%
    layout(title = "Evolución del consumo energético anual",
           xaxis = list(title = "Mes del año",
                        ticktext = list("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre"), 
                        tickvals = plot_line$`Date-MY`
                        
           )
           ,
           yaxis = list (title = "Energía (Varios-hora)"))

}) 
  




#### Plot de la comparación del consumo medio semanal a lo largo de los años #### 


DataMedia09 <- reactive({
  filter(Granularidad_dias, year == 2009  ) %>% group_by(weekday) %>% 
    summarize ( 
      Sub_metering_1 = mean(Sub_metering_1),
      Sub_metering_2 = mean(Sub_metering_2),
      Sub_metering_3 = mean(Sub_metering_3)
    )
})


DataMediaResto <- reactive({
  filter(Granularidad_dias, year != 2009  ) %>% group_by(weekday) %>% 
    summarize ( 
      Sub_metering_1 = mean(Sub_metering_1),
      Sub_metering_2 = mean(Sub_metering_2),
      Sub_metering_3 = mean(Sub_metering_3)
    )
})


output$ConsumoMedio <- renderPlotly({
  Media09 <- DataMedia09()
  EnergiaMedia <- DataMediaResto()
  plot_ly(EnergiaMedia, 
          x = ~EnergiaMedia$weekday,
          y = ~EnergiaMedia$Sub_metering_1,
          name = 'Cocina',  type = 'bar') %>% 
    add_trace(x~Media09$weekday,
              y = ~Media09$Sub_metering_1,
              name = "Cocina" , line=list(color='blue'), 
              type = 'scatter', 
              mode = 'lines+markers',showlegend = FALSE,
              marker = list(
                color = "blue"
              )) %>% 
    add_trace(x~EnergiaMedia$weekday,
              y = ~EnergiaMedia$Sub_metering_2,
              name = "Lavadero" ,
              type = 'bar') %>% 
    add_trace(x~Media09$weekday,
              y = ~Media09$Sub_metering_2,
              name = "Lavadero" , line=list(color='green'), 
              type = 'scatter',
              mode = 'lines+markers',showlegend = FALSE,
              marker = list(
                color = "green"
              )) %>% 
    add_trace(x~EnergiaMedia$weekday,
              y = ~EnergiaMedia$Sub_metering_3,
              name = 'Termo y AC' ,
              type = 'bar') %>% 
    add_trace(x~Media09$weekday,
              y = ~Media09$Sub_metering_3,
              name = 'Termo y AC' ,line=list(color='blueviolet'), 
              type = 'scatter', 
              mode = 'lines+markers',showlegend = FALSE,
              marker = list(
                color = "blueviolet"
              )) %>% 
    layout(title = "Comparación del consumo energético semanal medio \n Año 2009 vs años 2006-2008",
           xaxis = list(title = "Día de la semana", 
                        ticktext=list("Lunes", "Martes", "Miércoles", "Jueves","Viernes","Sábado","Domingo"),
                        tickvals=list(1,2,3,4,5,6,7)),
           yaxis = list (title = "Energía media (Vatios-hora)")
    )
}) 





# Media09 <- reactive({
# filter(Granularidad_dias, year == 2009  ) %>% group_by(weekday) %>% 
#     summarize ( 
#       Sub_metering_1 = mean(Sub_metering_1),
#       Sub_metering_2 = mean(Sub_metering_2),
#       Sub_metering_3 = mean(Sub_metering_3)
#     )
# })
  
  
#  EnergiaMediaSemanalGlobal <- reactive({
#   
#  filter(Granularidad_dias, year != 2009  ) %>% group_by(weekday) %>% 
#    summarize ( 
#      Sub_metering_1 = mean(Sub_metering_1),
#      Sub_metering_2 = mean(Sub_metering_2),
#      Sub_metering_3 = mean(Sub_metering_3)
#    ) 
#  })
#  
# output$ConsumoSemanalMedioGlobal <- renderPlotly({
#      
#      Media09 <- Media09()  
#      EnergiaMedia <-  EnergiaMediaSemanalGlobal()
#      
#      plot_ly(data = EnergiaMedia, 
#              x = ~EnergiaMedia$weekday,
#              y = ~EnergiaMedia$Sub_metering_1,
#              name = 'Cocina',  type = 'bar') %>% 
#        add_trace(x~Media09$weekday,
#                  y = ~Media09$Sub_metering_1,
#                  name = "Cocina" , line=list(color='blue'), 
#                  type = 'scatter', 
#                  mode = 'lines+markers',marker = list(
#                    color = "blue",showlegend = FALSE
#                  )) %>% 
#        add_trace(x~EnergiaMedia$weekday,
#                  y = ~EnergiaMedia$Sub_metering_2,
#                  name = "Lavadero" ,
#                  type = 'bar') %>% 
#        add_trace(x~Media09$weekday,
#                  y = ~Media09$Sub_metering_2,
#                  name = "Lavadero" , line=list(color='green'), 
#                  type = 'scatter',
#                  mode = 'lines+markers',marker = list(
#                    color = "green",showlegend = FALSE
#                  )) %>% 
#        add_trace(x~EnergiaMedia$weekday,
#                  y = ~EnergiaMedia$Sub_metering_3,
#                  name = 'Termo y AC' ,
#                  type = 'bar') %>% 
#        add_trace(x~Media09$weekday,
#                  y = ~Media09$Sub_metering_3,
#                  name = 'Termo y AC' ,line=list(color='blueviolet'), 
#                  type = 'scatter', 
#                  mode = 'lines+markers',marker = list(
#                    color = "blueviolet",
#                  )) %>% 
#        layout(title = "Comparación del consumo energético semanal medio \n Año 2009 vs años 2006-2008",
#               xaxis = list(title = "Día de la semana", 
#                            ticktext=list("Lunes", "Martes", "Miércoles", "Jueves","Viernes","Sábado","Domingo"),
#                            tickvals=list(1,2,3,4,5,6,7)),
#               yaxis = list (title = "Energía media (Vatios-hora)")
#        )
#    }) 




## Plot de la comparación del consumo energético Semanal ####

MediaSemana09 <- reactive({
  filter(Granularidad_dias, year == 2009 & week == input$Semana  )
})


UltimaSemana09_OtrosAnosG <- reactive({
  # Consumo total medio según dia.
  filter(Granularidad_dias, year != 2009 & week == input$Semana ) %>% 
    group_by(weekday) %>% 
    summarize ( 
      Sub_metering_1 = mean(Sub_metering_1),
      Sub_metering_2 = mean(Sub_metering_2),
      Sub_metering_3 = mean(Sub_metering_3)
    )
})



output$ConsumoSemana <- renderPlotly({
  
  UltimaSemana09 <- MediaSemana09()  
  UltimaSemana09_OtrosAños <-  UltimaSemana09_OtrosAnosG()
  
  plot_ly(UltimaSemana09, 
               x = ~UltimaSemana09$Date,
               y = ~UltimaSemana09$Sub_metering_1,
               name = 'Cocina', 
               type = 'scatter', mode = 'lines+markers',showlegend = FALSE) %>%
    add_trace(y = ~UltimaSemana09$Sub_metering_2, name = 'Lavadero',
              line=list(color='rgb(254, 237, 108)'),
              marker = list(color =   'rgb(243, 221, 63  )'),
              mode = 'lines+markers',showlegend = FALSE) %>%
    add_trace(y = ~UltimaSemana09$Sub_metering_3,
              name = 'Termo y AC', 
              line=list(color='rgb(150, 218, 108)'), 
              marker = list(color = 'rgb(150, 218, 108)'),
              mode = 'lines+markers',showlegend = FALSE) %>%
    add_trace(x~UltimaSemana09_OtrosAños$weekday,
              y = ~UltimaSemana09_OtrosAños$Sub_metering_1,
              name = 'Cocina' , marker = list(color = 'rgb(63, 148, 210)'),
              type = 'bar',showlegend = T) %>% 
    add_trace(x~UltimaSemana09_OtrosAños$weekday,
              y = ~UltimaSemana09_OtrosAños$Sub_metering_2,
              name = 'Lavadero' ,   marker = list(color = 'rgb(254, 237, 108)'),
              type = 'bar',showlegend = T) %>% 
    add_trace(x~UltimaSemana09_OtrosAños$weekday,
              y = ~UltimaSemana09_OtrosAños$Sub_metering_3,
              name = 'Termo y AC' ,   marker = list(color = 'rgb(150, 218, 108)'),
              type = 'bar',showlegend = T) %>% 
    layout(title = "Comparación del consumo energético semanal \n Año 2009 vs Años 2006-2008",
           xaxis = list(title = "Día de la semana"
           ),yaxis = list (title = "Energía (Vatios-Hora)"))
  
  
  
  
}) 

  


  





## Prueba ####

DataPrueba <- reactive({
 ungroup( Granularidad_meses )
})



output$Prueba2  <- renderPlotly({
  ds <- DataPrueba()
  
  fig <- plot_ly(ds,  type = 'scatter', mode ='lines' )
  fig <- fig %>% add_lines(x =~as.Date.character(  ds$`Date-MY` ) ,y = ~ds$Sub_metering_1, 
                           name = "Cocina"  ,line=list(color='rgb(254, 237, 108)'))
  fig <- fig %>% add_lines(x = ~as.Date.character(  ds$`Date-MY` ) ,y = ~ds$Sub_metering_2, 
                           name = "Lavadero" , line=list(color='rgb(199, 0, 57 )'))
  fig <- fig %>% add_lines(x = ~as.Date.character(  ds$`Date-MY` ),y = ~ds$Sub_metering_3,
                           name = "Calefacción & AC"  , line=list(color='rgb(150, 218, 108)'))
  fig <- fig %>% add_lines(x = ~as.Date.character(  ds$`Date-MY` ) ,y = ~ds$Global_active_power,
                           name = "Energía total", line=list(color=  'rgb(89, 118, 236  )')) 
  fig <- fig %>% layout( title='',
                         xaxis = list(title = "Fecha",
                                      rangeslider = list(type = "date"),
                                      rangeselector = list(
                                        buttons = list(
                                          list(
                                            count = 3,
                                            label = "3 mo",
                                            step = "month",
                                            stepmode = "forward"),
                                          list(
                                            count = 6,
                                            label = "6 mo",
                                            step = "month",
                                            stepmode = "forward"),
                                          list(
                                            count = 1,
                                            label = "1 yr",
                                            step = "year",
                                            stepmode = "forward"),
                                          list(step = "all")))), 
                         yaxis = list(title = "Energía (Varios-hora)") )
  fig
  
  
  fig


}) 

  
# Piecharts ####
### Pie TOTAL ####
# PieTotal ####


pieGlobalDatos <- reactive({
  Gda<- data.frame(
    
    energiaTotal<-sum(DatosCompletos$Global_active_power),
    sm1<-sum(DatosCompletos$Sub_metering_1),
    sm2<-sum(DatosCompletos$Sub_metering_2),
    sm3<-sum(DatosCompletos$Sub_metering_3),
    en2<-sum(DatosCompletos$energia2)
    
  ) %>% gather()
  
  data.frame(key=c("Cocina","Lavadero","Termo eléct & aire acond","Resto de la casa"),
             value=c(round((Gda[2,2]/Gda[1,2])*100,2),
                     round((Gda[3,2]/Gda[1,2])*100,2),
                     round((Gda[4,2]/Gda[1,2])*100,2),
                     round((Gda[5,2]/Gda[1,2])*100,2)
             ))
  
})

output$PieTotal <- renderPlotly({
  
  plot_Pie <-pieGlobalDatos() 
  plot_ly(plot_Pie,
          labels = ~key, values = ~value, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',showlegend = FALSE,
          marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                        line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = 'Porcentaje del consumo energético',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})




#### Pie Invierno y Verano ####
#   "PieVerano" , "PieInvierno"
#### Invierno ####


#### Data ####
DatosInvierno7 <- reactive({
  PorcMensualinvierno<-Granularidad_meses %>% 
    filter(month==1 | month==2) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual7Invierno<-PorcMensualinvierno[1,-1] %>% gather()
  Anual7Invierno$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual7Invierno
})


DatosInvierno8 <- reactive({
  PorcMensualinvierno<-Granularidad_meses %>% 
    filter(month==1 | month==2) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual8Invierno<-PorcMensualinvierno[2,-1] %>% gather()
  Anual8Invierno$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual8Invierno
})

DatosInvierno9 <- reactive({
  PorcMensualinvierno<-Granularidad_meses %>% 
    filter(month==1 | month==2) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual9Invierno<-PorcMensualinvierno[3,-1] %>% gather()
  Anual9Invierno$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual9Invierno
})

DatosInvierno10 <- reactive({
  PorcMensualinvierno<-Granularidad_meses %>% 
    filter(month==1 | month==2) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual10Invierno<-PorcMensualinvierno[4,-1] %>% gather()
  Anual10Invierno$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual10Invierno
})


#### Gráfico ####


output$PieInvierno1 <- renderPlotly({
  plot_PieInvierno7 <- DatosInvierno7() 
  plot_PieInvierno8 <- DatosInvierno8() 
  fig <- plot_ly()
  fig <- fig %>% add_pie(data = plot_PieInvierno7, labels = ~key, values = ~value,
                         title = 'Año 2007',
                         name = "Cut", domain = list(row = 0, column = 0), showlegend=FALSE,
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieInvierno8, labels = ~key, values = ~value, title = 'Año 2008',
                         name = "Color", domain = list(row = 1, column = 0), showlegend=FALSE,
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout(showlegend = T,
                        grid=list(rows=2, columns=1),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig

})

output$PieInvierno2 <- renderPlotly({

 plot_PieInvierno9 <- DatosInvierno9() 
 plot_PieInvierno10 <- DatosInvierno10() 
  
  
  fig <- plot_ly()
  fig <- fig %>% add_pie(data = plot_PieInvierno9, labels = ~key, values = ~value,
                         title = 'Año 2009',
                         name = "Cut", domain = list(row = 0, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieInvierno10, labels = ~key, values = ~value, title = 'Año 2010',
                         name = "Color", domain = list(row = 1, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout( showlegend = T,
                        grid=list(rows=2, columns=1),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})





#### Verano ####
#### Data ####

DatosVerano7 <- reactive({
  PorcMensualVerano<-Granularidad_meses %>% 
    filter(month==7 | month==8) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual7Verano<-PorcMensualVerano[1,-1] %>% gather()
  Anual7Verano$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual7Verano
})


DatosVerano8 <- reactive({
  PorcMensualVerano<-Granularidad_meses %>% 
    filter(month==8 | month==7) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual8Verano<-PorcMensualVerano[2,-1] %>% gather()
  Anual8Verano$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual8Verano
})

DatosVerano9 <- reactive({
  PorcMensualVerano<-Granularidad_meses %>% 
    filter(month==7 | month==8) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual9Verano<-PorcMensualVerano[3,-1] %>% gather()
  Anual9Verano$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual9Verano
})

DatosVerano10 <- reactive({
  PorcMensualVerano<-Granularidad_meses %>% 
    filter(month==7 | month==8.) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2)
    )
  Anual10Verano<-PorcMensualVerano[4,-1] %>% gather()
  Anual10Verano$key<-c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa")
  Anual10Verano
})



#### Gráfico ####

output$PieVerano1 <- renderPlotly({
  plot_PieVerano7 <- DatosInvierno7() 
  plot_PieVerano8 <- DatosInvierno8() 
  fig <- plot_ly()
  fig <- fig %>% add_pie(data = plot_PieVerano7, labels = ~key, values = ~value,
                         title = 'Año 2007',
                         name = "Cut", domain = list(row = 0, column = 0), showlegend=FALSE,
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieVerano8, labels = ~key, values = ~value, title = 'Año 2008',
                         name = "Color", domain = list(row = 1, column = 0), showlegend=FALSE,
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout(showlegend = T,
                        grid=list(rows=2, columns=1),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})

output$PieVerano2 <- renderPlotly({
  
  plot_PieVerano9 <-  DatosVerano9() 
  plot_PieVerano10 <- DatosVerano10() 
  
  
  fig <- plot_ly()
  fig <- fig %>% add_pie(data = plot_PieVerano9, labels = ~key, values = ~value,
                         title = 'Año 2009',
                         name = "Cut", domain = list(row = 0, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieVerano10, labels = ~key, values = ~value, title = 'Año 2010',
                         name = "Color", domain = list(row = 1, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% layout( showlegend = T,
                         grid=list(rows=2, columns=1),
                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})












## Pie Chart Anual ####
  pieAnualDatos <- reactive({
    Gda<- data.frame(Granularidad_dias %>% 
      filter(year ==  input$AnoPieAnual & month == input$MesPieAnual & day == input$DiaPieAnual ) %>% 
      select(c(7,8,9,10,11) ) %>% gather() ) 
    
    data.frame(key=c("Cocina","Lavadero","Termo eléct & aire acond","Resto de la casa"),
                                      value=c(round((Gda[3,2]/Gda[6,2])*100,2),
                                              round((Gda[4,2]/Gda[6,2])*100,2),
                                              round((Gda[5,2]/Gda[6,2])*100,2),
                                              round((Gda[7,2]/Gda[6,2])*100,2)
                                      ))
    
  })
  
  output$PieAnual <- renderPlotly({
    
    plot_Pie <- pieAnualDatos() 
    plot_ly(plot_Pie,
            labels = ~key, values = ~value, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',showlegend = FALSE,
            marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = 'Porcentaje del consumo energético un día concreto',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    
  })
  
  

  
  
## Pie mensual global ####
# input$MesAno  output$PieMensual

  
  DatosDeMes<- reactive({
    Mes <- data.frame(
    Granularidad_meses %>% group_by(month) %>% 
      summarize(
        Sub_metering_1=sum(Sub_metering_1),
        Sub_metering_2=sum(Sub_metering_2),
        Sub_metering_3=sum(Sub_metering_3),
        energia2=sum(energia2),
        Global_active_power=sum(Global_active_power)
      ) %>% filter(month==input$MesAno))
  
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa"),
                           value=c(round((Mes[1,2]/Mes[1,6])*100,2),
                                   round((Mes[1,3]/Mes[1,6])*100,2),
                                   round((Mes[1,4]/Mes[1,6])*100,2),
                                   round((Mes[1,5]/Mes[1,6])*100,2)
                           ))
  })
  
  output$PieMensual <- renderPlotly({
    PieMesDf<-DatosDeMes()
    fig <- plot_ly(PieMesDf, labels = ~key, values = ~value, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   hoverinfo = 'text',showlegend = FALSE,
                   marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                 line = list(color = '#FFFFFF', width = 1))
    )
    fig <- fig %>% layout( title = 'Porcentaje del consumo energético mensual',
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  
  
  
## Pie mensual por año ####
  # input$MesAno2  output$PieMensual2
  
  DatosDeMesYano<- reactive({
    Mes <- data.frame(
      Granularidad_meses %>% filter(year== input$AnoPie2) %>% 
        group_by(month) %>% 
        summarize(
          Sub_metering_1=sum(Sub_metering_1),
          Sub_metering_2=sum(Sub_metering_2),
          Sub_metering_3=sum(Sub_metering_3),
          energia2=sum(energia2),
          Global_active_power=sum(Global_active_power)
        ) %>% filter(month==input$MesAno2))
    
    data.frame(key=c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa"),
               value=c(round((Mes[1,2]/Mes[1,6])*100,2),
                       round((Mes[1,3]/Mes[1,6])*100,2),
                       round((Mes[1,4]/Mes[1,6])*100,2),
                       round((Mes[1,5]/Mes[1,6])*100,2)
               ))
  })
  
  output$PieMensualAno <- renderPlotly({
    PieMesDf<-DatosDeMesYano()
    fig <- plot_ly(PieMesDf, labels = ~key, values = ~value, type = 'pie',
                   textposition = 'inside',
                   textinfo = 'label+percent',
                   hoverinfo = 'text',showlegend = FALSE,
                   marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                 line = list(color = '#FFFFFF', width = 1))
                   )
    fig <- fig %>% layout( title = 'Porcentaje del consumo energético mensual para un año concreto',
                           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  
  
### Pie chart por horas ####
#### Primera ####

# inputs DiaPieAnualPorHora MesPieAnualPorHora AnoPieAnualPorHora
  
  pieAnualHoraDatos1 <- reactive({
    Tit<-c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Energía total","Resto de la casa")
    Uno<-Granularidad_horas  %>% filter( Date==input$DatePie &
                                          (hour==0 | hour==1 | hour==2 | hour==3 | hour==4 | hour==5  ) ) 
    Uno<-data.frame(Tit, apply(Uno[,c(11,12,13,14,15)],2,sum) )
    data.frame(key=c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Resto de la casa"),
                    value=c(round((Uno[1,2]/Uno[4,2])*100,2),
                            round((Uno[2,2]/Uno[4,2])*100,2),
                            round((Uno[3,2]/Uno[4,2])*100,2),
                            round((Uno[5,2]/Uno[4,2])*100,2)
                    ))
  })

  
  

  
  output$PieAnualPorHora1 <- renderPlotly({
    
    plot_PieHoras1 <- pieAnualHoraDatos1() 
    plot_ly(plot_PieHoras1,
            labels = ~key, values = ~value, type = 'pie',title = '0-5 h',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',showlegend = FALSE) %>%
      layout(
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })
  
#### Segunda ####

# inputs DiaPieAnualPorHora MesPieAnualPorHora AnoPieAnualPorHora

# pieAnualHoraDatos2 <- reactive({
#   Tit<-c("Cocina","Lavadero","Termo eléctrico","global_active_power","energia2")
#   Dos<-Granularidad_horas  %>% filter(year == input$AnoPieAnualPorHora &
#                                         day == input$DiaPieAnualPorHora & 
#                                         month == input$MesPieAnualPorHora &
#                                         (hour==7 | hour==8 | hour==9 | hour==10 | hour==11 | hour==6)  ) 
#   Dos<-data.frame(Tit, apply(Dos[,c(11,12,13,14,15)],2,sum) )
#   data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
#              value=c(round((Dos[1,2]/Dos[4,2])*100,2),
#                      round((Dos[2,2]/Dos[4,2])*100,2),
#                      round((Dos[3,2]/Dos[4,2])*100,2),
#                      round((Dos[5,2]/Dos[4,2])*100,2)
#              ))
# })
# 
  pieAnualHoraDatos2 <- reactive({
    Tit<-c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Energía total","Resto de la casa")
    Dos<-Granularidad_horas  %>% filter(   Date==input$DatePie &
                                          (hour==7 | hour==8 | hour==9 | hour==10 | hour==11 | hour==6)  ) 
    Dos<-data.frame(Tit, apply(Dos[,c(11,12,13,14,15)],2,sum) )
    data.frame(key=c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Resto de la casa"),
               value=c(round((Dos[1,2]/Dos[4,2])*100,2),
                       round((Dos[2,2]/Dos[4,2])*100,2),
                       round((Dos[3,2]/Dos[4,2])*100,2),
                       round((Dos[5,2]/Dos[4,2])*100,2)
               ))
  })




output$PieAnualPorHora2 <- renderPlotly({
  
  plot_PieHoras2 <- pieAnualHoraDatos2() 
  plot_ly(plot_PieHoras2,
          labels = ~key, values = ~value, type = 'pie',title = '6-11 h',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',showlegend = FALSE) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})  

  
#### Tercera ####

# inputs DiaPieAnualPorHora MesPieAnualPorHora AnoPieAnualPorHora

pieAnualHoraDatos3 <- reactive({
  Tit<-c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Energía total","Resto de la casa")
  Tres<-Granularidad_horas  %>% filter( Date==input$DatePie &
                                         (hour==13 | hour==14 | hour==15 | hour==16 | hour==17 | hour==12)  ) 
  Tres<-data.frame(Tit, apply(Tres[,c(11,12,13,14,15)],2,sum) )
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Resto de la casa"),
             value=c(round((Tres[1,2]/Tres[4,2])*100,2),
                     round((Tres[2,2]/Tres[4,2])*100,2),
                     round((Tres[3,2]/Tres[4,2])*100,2),
                     round((Tres[5,2]/Tres[4,2])*100,2)
             ))
})





output$PieAnualPorHora3 <- renderPlotly({
  
  plot_PieHoras3 <- pieAnualHoraDatos3() 
  plot_ly(plot_PieHoras3,
          labels = ~key, values = ~value, type = 'pie',title = '12-17 h',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',showlegend = FALSE) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})  

  

  
#### Cuarta ####

# inputs DiaPieAnualPorHora MesPieAnualPorHora AnoPieAnualPorHora

pieAnualHoraDatos4 <- reactive({
  Tit<-c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Energía total","Resto de la casa")
  Cuatro<-Granularidad_horas  %>% filter( Date==input$DatePie &
                                         (hour==18 | hour==19 | hour==20 | hour==21 | hour==22 | hour==23)  ) 
  Cuatro<-data.frame(Tit, apply(Cuatro[,c(11,12,13,14,15)],2,sum) )
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico y aire acondicionado","Resto de la casa"),
             value=c(round((Cuatro[1,2]/Cuatro[4,2])*100,2),
                     round((Cuatro[2,2]/Cuatro[4,2])*100,2),
                     round((Cuatro[3,2]/Cuatro[4,2])*100,2),
                     round((Cuatro[5,2]/Cuatro[4,2])*100,2)
             ))
})





output$PieAnualPorHora4 <- renderPlotly({
  
  plot_PieHoras4 <- pieAnualHoraDatos4() 
  plot_ly(plot_PieHoras4,
          labels = ~key, values = ~value, type = 'pie',title = '18-23 h',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',showlegend = FALSE
         
          ) %>%
    layout(
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})  



output$Prueba <- renderPlotly({
  plot_PieHoras1 <- pieAnualHoraDatos1() 
  plot_PieHoras2 <- pieAnualHoraDatos2() 
  plot_PieHoras3 <- pieAnualHoraDatos3() 
  plot_PieHoras4 <- pieAnualHoraDatos4() 
  
  fig <- plot_ly()
  fig <- fig %>% add_pie(data = plot_PieHoras1, labels = ~key, values = ~value,
                         title = '0-5 h',
                         name = "Cut", domain = list(row = 0, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieHoras2, labels = ~key, values = ~value, title = '6-12 h',
                         name = "Color", domain = list(row = 0, column = 1),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieHoras3, ~key, values = ~value, title = '13-17 h',
                         name = "Clarity", domain = list(row = 1, column = 0),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1)))
  fig <- fig %>% add_pie(data = plot_PieHoras4, ~key, values = ~value, title = '18-23 h',
                         name = "Clarity", domain = list(row = 1, column = 1),
                         marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                                       line = list(color = '#FFFFFF', width = 1))
                         )
  fig <- fig %>% layout( showlegend = T,
                        grid=list(rows=2, columns=2),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})






### Pie Un año completo ####

# output$PieUnAno,input$AnoPie

pieAnualS <- reactive({
  
  GdaII <- data.frame(
    Granularidad_meses %>% group_by(year) %>% filter(year==input$AnoPie) %>% 
    summarize(
      Sub_metering_1=sum(Sub_metering_1),
      Sub_metering_2=sum(Sub_metering_2),
      Sub_metering_3=sum(Sub_metering_3),
      energia2=sum(energia2),
      Global_active_power=sum(Global_active_power)
    ) %>% 
      gather() )
  
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico y AC","Resto de la casa"),
             value=c(round((GdaII[2,2]/GdaII[6,2])*100,2),
                     round((GdaII[3,2]/GdaII[6,2])*100,2),
                     round((GdaII[4,2]/GdaII[6,2])*100,2),
                     round((GdaII[5,2]/GdaII[6,2])*100,2)
             ))
  
})

output$PieUnAno <- renderPlotly({
  
  plot_PieS <- pieAnualS()
  plot_ly(plot_PieS,
          labels = ~key, values = ~value, type = 'pie',
          textposition = 'inside',
          textinfo = 'label+percent',
          hoverinfo = 'text',showlegend = FALSE,
          marker = list(colors = c("#CD3333","#EE7621","#548B54","#1874CD"),
                        line = list(color = '#FFFFFF', width = 1))) %>%
    layout(title = 'Porcentaje del consumo energético anual',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  
})







### Pronostico ####



# output$PronosticoMensual


# output$PronosticoMensual



dfPlotly <- reactive({
  
  dfPlotlyPronosticoMensual
  
})


output$PronosticoMensual <- renderPlotly({
  dfPlotly<-dfPlotly()
  plot_ly(dfPlotly,
          x = ~Fecha, 
          y = ~Cocina,
          name = "Cocina" , type='scatter',  mode='lines'  ) %>% 
    add_trace( y = ~dfPlotly$Lavadero,name = "Lavadero" , mode='lines')%>%
  #   add_trace( y = ~dfPlotly$AireTermo, name = "Aire acondicionado y termo" , mode='lines') %>%
    layout(title = "Pronóstico del consumo energético de la cocina para el año 2011",
           xaxis = list(title = "Mes",
                        ticktext = c("Dec 2010"	,"Jan 2011"	,"Feb 2011"	,"Mar 2011"	,"Apr 2011"	 ,"May 2011"	,"Jun 2011"	,"Jul 2011"	,"Aug 2011"	,"Sep 2011","Oct 2011","Nov 2011","Dec # 2011"), tickvals = dfPlotly$Fecha)
           , yaxis = list (title = "Energía (Varios-hora)"))
})




dfPlotlyAgosto <- reactive({
  
  dfPlotlyPronosticoAgosto
  
})


output$PronosticoAgosto <- renderPlotly({
  
  dfPlotlyPronosticoAgosto<-dfPlotlyAgosto()
  plot_ly(dfPlotlyPronosticoAgosto,
          x = ~dfPlotlyPronosticoAgosto$Fecha, 
          y = ~dfPlotlyPronosticoAgosto$Cocina,
          name = "Cocina" , type='scatter',  mode='lines'  ) %>% 
 #    add_trace( y = ~dfPlotlyPronosticoAgosto$Lavadero,name = "Lavadero" , mode='lines')%>%
    add_trace( y = ~dfPlotlyPronosticoAgosto$AireTermo, name = "Aire acondicionado y termo" , mode='lines') %>%
    layout(title = "Pronóstico del consumo energético para Agosto del año 2011",
           xaxis = list(title = "Mes", ticktext = paste(1:31,"Ago"), tickvals = dfPlotlyPronosticoAgosto$Fecha)
           , yaxis = list (title = "Energía (Varios-hora)"))
})


dfPlotlyEnero<- reactive({
  
  dfPlotlyPronosticoEnero
  
})


output$PronosticoEnero <- renderPlotly({
  
  dfPlotlyPronosticoEnero<-dfPlotlyEnero()
  plot_ly(dfPlotlyPronosticoEnero,
          x = ~dfPlotlyPronosticoEnero$Fecha, 
          y = ~dfPlotlyPronosticoEnero$Cocina,
          name = "Cocina" , type='scatter',  mode='lines'  ) %>% 
    #  add_trace( y = ~dfPlotlyPronosticoEnero$Lavadero,name = "Lavadero" , mode='lines')%>%
    add_trace( y = ~dfPlotlyPronosticoEnero$AireTermo, name = "Aire acondicionado y termo" , mode='lines') %>% 
    layout(title = "Pronóstico del consumo energético para Enero del año 2011",
           xaxis = list(title = "Día del mes",
                        ticktext = c(1:31), tickvals = dfPlotlyPronosticoEnero$Fecha)
           , yaxis = list (title = "Energía (Varios-hora)"))
})


dfPlotlySemanalMedio<- reactive({
  
  dfSemanalMedio
  
})


output$PronosticoSemanalMedio<- renderPlotly({
  plot_ly(dfSemanalMedio,
          x = ~Fecha, 
          y = ~Cocina,
          name = "Cocina" , type='scatter',  mode='lines'  ) %>% 
    add_trace( y = ~dfSemanalMedio$Lavadero,name = "Lavadero" , mode='lines')%>%
    add_trace( y = ~dfSemanalMedio$AireTermo, name = "Aire acondicionado y termo" , mode='lines') %>%
    layout(title = "Pronóstico del consumo energético medio según día de la semana para el año 2011",
           xaxis = list(title = "Día de la semana",
                        ticktext=c("Lunes","Martes","Miércoles","Jueves","Viernes","Sábado","Domingo"),
                        tickvals=dfSemanalMedio$Fecha)
           , yaxis = list (title = "Energía (Varios-hora)"))
})

}
