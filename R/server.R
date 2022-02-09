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
  
  
## Plot del consumo anual
  
  PorcData <- reactive({
    Granularidad_meses %>% 
      filter(year == input$AnoPorcentaje) 
  })
  
  
  
output$ConsumoMensual <- renderPlotly({
  
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


UltimaSemana09_OtrosAñosG <- reactive({
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
  UltimaSemana09_OtrosAños <-  UltimaSemana09_OtrosAñosG()
  
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

  


  



# Piecharts ####


## Pie Chart Anual 
  pieAnualDatos <- reactive({
    Gda<- data.frame(Granularidad_dias %>% 
      filter(year ==  input$AnoPieAnual & month == input$MesPieAnual & day == input$DiaPieAnual ) %>% 
      select(c(7,8,9,10,11) ) %>% gather() ) 
    
    data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
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
            hoverinfo = 'text',showlegend = FALSE) %>%
      layout(title = 'Porcentaje del consumo energético',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

    
  })
  
### Pie chart por horas ####
#### Primera ####

# inputs DiaPieAnualPorHora MesPieAnualPorHora AnoPieAnualPorHora
  
  pieAnualHoraDatos1 <- reactive({
    Tit<-c("Cocina","Lavadero","Termo eléctrico","global_active_power","energia2")
    Uno<-Granularidad_horas  %>% filter(year == input$AnoPieAnualPorHora &
                                          day == input$DiaPieAnualPorHora & 
                                          month == input$MesPieAnualPorHora &
                                          (hour==0 | hour==1 | hour==2 | hour==3 | hour==4 | hour==5  ) ) 
    Uno<-data.frame(Tit, apply(Uno[,c(11,12,13,14,15)],2,sum) )
    data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
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

pieAnualHoraDatos2 <- reactive({
  Tit<-c("Cocina","Lavadero","Termo eléctrico","global_active_power","energia2")
  Dos<-Granularidad_horas  %>% filter(year == input$AnoPieAnualPorHora &
                                        day == input$DiaPieAnualPorHora & 
                                        month == input$MesPieAnualPorHora &
                                        (hour==7 | hour==8 | hour==9 | hour==10 | hour==11 | hour==6)  ) 
  Dos<-data.frame(Tit, apply(Dos[,c(11,12,13,14,15)],2,sum) )
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
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
  Tit<-c("Cocina","Lavadero","Termo eléctrico","global_active_power","energia2")
  Tres<-Granularidad_horas  %>% filter(year == input$AnoPieAnualPorHora &
                                        day == input$DiaPieAnualPorHora & 
                                        month == input$MesPieAnualPorHora &
                                         (hour==13 | hour==14 | hour==15 | hour==16 | hour==17 | hour==12)  ) 
  Tres<-data.frame(Tit, apply(Tres[,c(11,12,13,14,15)],2,sum) )
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
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
  Tit<-c("Cocina","Lavadero","Termo eléctrico","global_active_power","energia2")
  Cuatro<-Granularidad_horas  %>% filter(year == input$AnoPieAnualPorHora &
                                         day == input$DiaPieAnualPorHora & 
                                         month == input$MesPieAnualPorHora &
                                         (hour==18 | hour==19 | hour==20 | hour==21 | hour==22 | hour==23)  ) 
  Cuatro<-data.frame(Tit, apply(Cuatro[,c(11,12,13,14,15)],2,sum) )
  data.frame(key=c("Cocina","Lavadero","Termo eléctrico","No submeterizados"),
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
          hoverinfo = 'text',showlegend = FALSE) %>%
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
                         name = "Cut", domain = list(row = 0, column = 0))
  fig <- fig %>% add_pie(data = plot_PieHoras2, labels = ~key, values = ~value, title = '6-12 h',
                         name = "Color", domain = list(row = 0, column = 1))
  fig <- fig %>% add_pie(data = plot_PieHoras3, ~key, values = ~value, title = '13-17 h',
                         name = "Clarity", domain = list(row = 1, column = 0))
  fig <- fig %>% add_pie(data = plot_PieHoras4, ~key, values = ~value, title = '18-23 h',
                         name = "Clarity", domain = list(row = 1, column = 1))
  fig <- fig %>% layout( showlegend = T,
                        grid=list(rows=2, columns=2),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  
  fig
  
})




}