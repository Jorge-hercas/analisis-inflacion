

function(input, output, session){
  
  x <- reactive({
    
    datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      ) |> 
      tidyr::pivot_longer(names_to = "tipo_inf", cols = c(subyacente,no_subyacente))
    
  })
  
  y <- reactive({
    
    datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      )  |> 
      mutate(
        subyacente_difs = (subyacente-lag(subyacente))/lag(subyacente),
        no_subyacente_difs = (no_subyacente - lag(no_subyacente))/lag(no_subyacente)
      ) |> 
      select(date, subyacente_difs, no_subyacente_difs) |> 
      tidyr::pivot_longer(names_to = "tipo_inf", cols = c(subyacente_difs,no_subyacente_difs))
    
  })
  
  
  output$diferencias <- renderEcharts4r({
    
    x <- 
    datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      ) |> 
      mutate(
        subyacente_difs = (subyacente-lag(subyacente))/lag(subyacente),
        no_subyacente_difs = (no_subyacente - lag(no_subyacente))/lag(no_subyacente),
        subyacente_difs_an = (1 + (subyacente_difs) / 12)^12 - 1,
        no_subyacente_difs_an = (1 + (no_subyacente_difs) / 12)^12 - 1
      ) |> 
      mutate_all(function(x) ifelse(is.infinite(x), 0, x)) |> 
      mutate(date = as_date(date))
    
    
    if (input$dif_perc == "Mensuales con respecto al último dato anterior"){
      
      x |> 
        e_charts(date) |> 
        e_bar(subyacente_difs, name = "Inf. subyacente") |> 
        e_bar(no_subyacente_difs, name = "Inf. no subyacente") |> 
        e_theme("auritus") |> 
        e_title("Inflación en México", 
                "Diferencias porcentuales respecto al periodo anterior", 
                left = "center",
                textStyle = list(
                  color = "#e3d6b1",
                  fontFamily = "oswald"
                )
        ) |> 
        e_tooltip(trigger = "axis",
                  textStyle = list(
                    fontFamily = "oswald"
                  )
        ) |> 
        e_legend(orient = "horizontal", bottom = 0,
                 textStyle = list(
                   color = "#e3d6b1",
                   fontFamily = "oswald"
                 )
        ) |> 
        e_mark_line(
          data = list(yAxis = max(x$subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Máximo sub") |> 
        e_mark_line(
          data = list(yAxis = min(x$subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Mínimo sub") |> 
        e_mark_line(
          data = list(yAxis = mean(x$subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Promedio sub") |> 
        e_mark_line(
          data = list(yAxis = max(x$no_subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Máximo no sub") |> 
        e_mark_line(
          data = list(yAxis = min(x$no_subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Mínimo no sub") |> 
        e_mark_line(
          data = list(yAxis = mean(x$no_subyacente_difs, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Promedio no sub") |> 
        e_text_style(color = "gray", font = "oswald") |> 
        e_color(color = c( "#8a3b3d", "#263b78")) |> 
        e_toolbox_feature(
          feature = "dataZoom"
        ) 
      
    }else if (input$dif_perc == "Mensuales anualizados"){
      
      x |> 
        e_charts(date) |> 
        e_bar(subyacente_difs_an, name = "Inf. subyacente") |> 
        e_bar(no_subyacente_difs_an, name = "Inf. no subyacente") |> 
        e_theme("auritus") |> 
        e_title("Inflación en México", 
                "Diferencias porcentuales en términos anualizados", left = "center",
                textStyle = list(
                  color = "#e3d6b1",
                  fontFamily = "oswald"
                )
        ) |> 
        e_tooltip(trigger = "axis",
                  textStyle = list(
                    fontFamily = "oswald"
                  )
        ) |> 
        e_legend(orient = "horizontal", bottom = 0,
                 textStyle = list(
                   color = "#e3d6b1",
                   fontFamily = "oswald"
                 )
        ) |> 
        e_mark_line(
          data = list(yAxis = max(x$subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Máximo sub") |> 
        e_mark_line(
          data = list(yAxis = min(x$subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Mínimo sub") |> 
        e_mark_line(
          data = list(yAxis = mean(x$subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#8a3b3d"),
          symbol = "none",
          title = "Promedio sub") |> 
        e_mark_line(
          data = list(yAxis = max(x$no_subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Máximo no sub") |> 
        e_mark_line(
          data = list(yAxis = min(x$no_subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Mínimo no sub") |> 
        e_mark_line(
          data = list(yAxis = mean(x$no_subyacente_difs_an, na.rm = T)),
          lineStyle = list(color = "#263b78"),
          symbol = "none",
          title = "Promedio no sub") |> 
        e_text_style(color = "gray", font = "oswald") |> 
        e_color(color = c( "#8a3b3d", "#263b78")) |> 
        e_toolbox_feature(
          feature = "dataZoom"
        ) 
      
    }
    
  }) |> 
    bindEvent(input$in_plot)
  
  output$comparativo <- renderEcharts4r({
    
    x <- datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      ) 
    
    
    x |> 
      e_charts(date) |> 
      e_bar(subyacente, name = "Inf. subyacente") |> 
      e_bar(no_subyacente, name = "Inf. no subyacente") |> 
      e_theme("auritus") |> 
      e_title("Inflación en México", 
              "Variaciones porcentuales", left = "center",
              textStyle = list(
                color = "#e3d6b1",
                fontFamily = "oswald"
              )
      ) |> 
      e_tooltip(trigger = "axis",
                textStyle = list(
                  fontFamily = "oswald"
                )
      ) |> 
      e_legend(orient = "horizontal", bottom = 0,
               textStyle = list(
                 color = "#e3d6b1",
                 fontFamily = "oswald"
               )
      ) |> 
      e_text_style(color = "gray", font = "oswald") |> 
      e_mark_line(
        data = list(yAxis = max(x$subyacente, na.rm = T)),
        lineStyle = list(color = "#8a3b3d"),
        symbol = "none",
        title = "Máximo sub") |> 
      e_mark_line(
        data = list(yAxis = min(x$subyacente, na.rm = T)),
        lineStyle = list(color = "#8a3b3d"),
        symbol = "none",
        title = "Mínimo sub") |> 
      e_mark_line(
        data = list(yAxis = mean(x$subyacente, na.rm = T)),
        lineStyle = list(color = "#8a3b3d"),
        symbol = "none",
        title = "Promedio sub") |> 
      e_mark_line(
        data = list(yAxis = max(x$no_subyacente, na.rm = T)),
        lineStyle = list(color = "#263b78"),
        symbol = "none",
        title = "Máximo no sub") |> 
      e_mark_line(
        data = list(yAxis = min(x$no_subyacente, na.rm = T)),
        lineStyle = list(color = "#263b78"),
        symbol = "none",
        title = "Mínimo no sub") |> 
      e_mark_line(
        data = list(yAxis = mean(x$no_subyacente, na.rm = T)),
        lineStyle = list(color = "#263b78"),
        symbol = "none",
        title = "Promedio no sub") |> 
      e_mark_line(
        data = list(xAxis = as.Date(tail(x$date[x$subyacente >= var(x$subyacente)*2],1)) ),
        lineStyle = list(color = "#263b78"),
        symbol = "none",
        title = "Variación *2 var") |> 
      e_mark_line(
        data = list(xAxis = as.Date("2018-12-01") ),
        lineStyle = list(color = "#263b78"),
        symbol = "none",
        title = "Último cambio de gobierno federal") |> 
      e_color(color = c( "#8a3b3d", "#263b78")) |> 
      e_toolbox_feature(
        feature = "dataZoom"
      )
      
    
  }) |> 
    bindEvent(input$in_plot)
  
  output$graficos <- renderUI({
    
    if (input$visual == "Comparativo"){
      column(width = 12,
             echarts4rOutput("comparativo", height = 500)
      )
    }else if (input$visual == "Diferencias"){
      column(width = 12,
             echarts4rOutput("diferencias", height = 500)
      )
    }else{
      column(width = 12,
             column(width = 6,
                    echarts4rOutput("comparativo", height = 500)
             ),
             column(width = 6,
                    echarts4rOutput("diferencias", height = 500)
             )
             
             )
    }
    
    
  }) |> 
    bindEvent(input$in_plot)
  
  output$graficos_componentes <- renderUI({
    
    if (input$visual == "Comparativo"){
      column(width = 12,
             echarts4rOutput("comparativo_comps", height = 500)
      )
    }else if (input$visual == "Diferencias"){
      column(width = 12,
             echarts4rOutput("diferencias_comps", height = 500)
      )
    }else{
      column(width = 12,
             column(width = 6,
                    echarts4rOutput("comparativo_comps", height = 500)
             ),
             column(width = 6,
                    echarts4rOutput("diferencias_comps", height = 500)
             )
             
      )
    }
    
    
  }) |> 
    bindEvent(input$in_plot)
  
  
  datos_componentes <- reactive({
    
    componentes |> 
      tidyr::pivot_longer(!Fecha, names_to = "Componente", values_to = "Valor") |> 
      filter(between(Fecha,min( input$fechas), max( input$fechas)) & Componente %in% input$comps_sel) |> 
      group_by(Componente) |> 
      mutate(inflacion = ((Valor-lag(Valor))/lag(Valor))*100) |> 
      mutate(diferencias = inflacion - lag(inflacion),
             Fecha = floor_date(Fecha, input$red_fechas)) |> 
      group_by(Componente, Fecha) |> 
      summarise(inflacion = mean(inflacion, na.rm = T),
                diferencias = mean(diferencias, na.rm = T)) |> 
      group_by(Componente)
    
  })
  
  
  output$comparativo_comps <- renderEcharts4r({
    
    datos_componentes() |> 
      e_charts(Fecha) |> 
      e_bar(inflacion, symbol = "none") |> 
      e_theme("auritus") |> 
      e_title("Inflación en México", 
              "Observaciones quincenales", left = "center",
              textStyle = list(
                color = "#e3d6b1",
                fontFamily = "oswald")) |> 
      e_tooltip(trigger = "axis",
                textStyle = list(
                  fontFamily = "oswald"
                )) |> 
      e_legend(orient = "horizontal", bottom = 0,
               textStyle = list(
                 color = "#e3d6b1",
                 fontFamily = "oswald"
               )) |> 
      e_text_style(color = "gray", font = "oswald") |> 
      e_color(color = c( "#003049", "#d62828", "#f77f00", "#fcbf49")) |> 
      e_toolbox_feature(
        feature = "dataZoom"
      )
    
  }) |> 
    bindEvent(input$in_plot)
  
  output$diferencias_comps <- renderEcharts4r({
    
    datos_componentes() |> 
      e_charts(Fecha) |> 
      e_bar(diferencias, symbol = "none") |> 
      e_theme("auritus") |> 
      e_title("Inflación en México", 
              "Diferencias porcentuales", left = "center",
              textStyle = list(
                color = "#e3d6b1",
                fontFamily = "oswald")) |> 
      e_tooltip(trigger = "axis",
                textStyle = list(
                  fontFamily = "oswald"
                )) |> 
      e_legend(orient = "horizontal", bottom = 0,
               textStyle = list(
                 color = "#e3d6b1",
                 fontFamily = "oswald"
               )) |> 
      e_text_style(color = "gray", font = "oswald") |> 
      e_color(color = c( "#003049", "#d62828", "#f77f00", "#fcbf49")) |> 
      e_toolbox_feature(
        feature = "dataZoom"
      )
    
  }) |> 
    bindEvent(input$in_plot)
  
  
  output$download <- downloadHandler(
    filename = function(){
      paste0("Descarga ", today(), ".xlsx")
    },
    content = function(file){
      
      wb_workbook() |> 
        wb_add_worksheet("Inflación. Evol", gridLines = F, 
                         tabColor = RColorBrewer::brewer.pal(5,"YlOrRd")[1]) |> 
        wb_add_mschart(
          dims = "G5", graph = ms_barchart(
            x(), x = "date", y = "value", group = "tipo_inf", asis = F) |> 
            chart_ax_x(num_fmt = "[$-es-ES]mmm yyyy") |> 
            chart_labels(title = "Inflación observada por componentes")) |> 
        wb_add_worksheet("Inflación. Difs", gridLines = F, 
                         tabColor = RColorBrewer::brewer.pal(5,"YlOrRd")[1]) |> 
        wb_add_mschart(
          dims = "G5", graph = ms_barchart(
            y(), x = "date", y = "value", group = "tipo_inf", asis = F) |> 
            chart_ax_x(num_fmt = "[$-es-ES]mmm yyyy") |> 
            chart_labels(title = "Difs en inflación observada por componentes")) |> 
        wb_save(path = file)
      
    }
  )
  
  
  
}

