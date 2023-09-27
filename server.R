

function(input, output, session){
  
  
  output$diferencias <- renderEcharts4r({
    
    datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      ) |> 
      mutate(
        subyacente_difs = subyacente-lag(subyacente),
        no_subyacente_difs = no_subyacente - lag(no_subyacente)
      ) |> 
      e_charts(date) |> 
      e_bar(subyacente_difs, name = "Inf. subyacente") |> 
      e_bar(no_subyacente_difs, name = "Inf. no subyacente") |> 
      e_theme("auritus") |> 
      e_title("Inflación en México", 
              "Diferencias porcentuales", left = "center",
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
      e_color(color = c( "#8a3b3d", "#263b78")) |> 
      e_toolbox_feature(
        feature = "dataZoom"
      )
    
  })
  
  output$comparativo <- renderEcharts4r({
    
    datos |>
      filter(between(date,min( input$fechas), max( input$fechas))) |> 
      mutate(date = floor_date(date, input$red_fechas)) |> 
      group_by(date) |> 
      summarise(
        subyacente = mean(subyacente),
        no_subyacente = mean(no_subyacente)
      ) |> 
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
        data = list(yAxis = 3),
        lineStyle = list(color = "#5e8dad"),
        symbol = "none",
        title = "Máximo") |> 
      e_color(color = c( "#8a3b3d", "#263b78")) |> 
      e_toolbox_feature(
        feature = "dataZoom"
      )
      
    
  })
  
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
    
    
  })
  
}

