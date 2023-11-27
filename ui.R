

fluidPage(
  useBs4Dash(),
  add_busy_spinner(spin = "fulfilling-bouncing-circle", position = "bottom-right"),
  gfonts::use_pkg_gfont("oswald"),
  chooseSliderSkin("Flat"),
  setSliderColor("gray", 1),
  setBackgroundColor("#546073"),
  tags$head(
    tags$style(HTML("
      .barra {
            height: 50px;
            background: linear-gradient(to right, rgba(0, 0, 0, 0.8), transparent);}
      .linea-punteada {
            border-top: 1px dashed #e3d6b1; 
            width: 100%; 
            }              
                    "))),
  fluidRow(
    column(width = 12),
    br(),
    column(width = 10,
           div(class = "barra")
    ),
    column(width = 2,
           align = "center",
           img(src = "https://icons.veryicon.com/png/o/miscellaneous/dist-icon-20-outlined/plot-ratio.png",height="50px")),
    column(width = 4, 
           h1(typed(c("INFLACIÓN EN MÉXICO", "ANÁLISIS DE LA INFLACIÓN"),typeSpeed = 20,backSpeed = 20), id = "titlepanel"),
           tags$style(HTML("#titlepanel{color: #e3d6b1;
                           font-size: 50px;
                           }")),
           
           ),
    column(width = 12, 
           h1("SEGUIMIENTO Y ESTIMACIONES", id = "subpanel"),
           tags$style(HTML("#subpanel{color: #e3d6b1;
                           font-size: 30px;}
                           #texto_in{color: #e3d6b1;}")),
           p(HTML(
             "<i>El propósito fundamental de esta aplicación es llevar a cabo un 
             minucioso seguimiento de los niveles de inflación, mediante la 
             meticulosa comparación de los datos recopilados por la 
             Reserva Federal de los Estados Unidos (FRED) con 
             los obtenidos por el Banco de México (BANXICO). Esta 
             herramienta se ha diseñado con la finalidad de proporcionar 
             una visión exhaustiva y precisa de la evolución de la inflación, 
             permitiendo así un análisis detallado de las tendencias 
             económicas y la toma de decisiones financieras de la empresa.</i>"
           ), id = "texto_in")
    ),
    column(width = 12, hr(class = "linea-punteada")),
    column(width = 3, 
           align = "center",
           titlePanel("Sobre los datos de inflación"),
           br(),
           airDatepickerInput(
             view = "years",
             minView = "years",
      inputId = "fechas", label = "Rango de fechas a visualizar",
      range = T, multiple = T,
      value = c(min(datos$date),max(datos$date))
    ),
    pickerInput(
      "red_fechas","¿Quieres redondear fechas?",
      choices = c(
        "Mensual" = "months",
        "Bimestral" = "bimonth",
        "Cuatrimestre" = "quarter",
        "Medio año" = "halfyear",
        "Anual" = "year"
      ),
      selected = "months"
    ),
    pickerInput(
      "dif_perc","Tipos de diferencias porcentuales a visualizar",
      choices = c(
        "Mensuales anualizados",
        "Quincenales anualizados",
        "Mensuales con respecto al último dato anterior",
        "Quincenales con respecto al último dato anterior"
      ),
      selected = "months"
    ),
    awesomeRadio(
      inputId = "visual",
      label = "Información a visualizar", 
      choices = c("Comparativo", "Diferencias", "Ambos"),
      selected = "Comparativo", inline = T
    ),
    actionButton("in_plot", "Generar gráfico", icon = icon("chart-line")),
    downloadButton("download", "Descargar datos", icon = icon("file-excel")),
    br(),br(),
    actionButton("refresh", "Refrescar los datos", icon = icon("rotate-right")),
    ),
    column(width = 9,
           tabsetPanel(
             tabPanel("Subyacente vs no subyacente",
                      br(),
                      fluidRow(
                        column(width = 4,
                               bs4ValueBox(
                                 width = 12,
                                 value = "Mínimo",
                                 subtitle = countupOutput("minimo"),
                                 color = "gray",
                                 icon = icon("arrow-down")
                               )
                        ),
                        column(width = 4,
                               bs4ValueBox(
                                 width = 12,
                                 value = "Máximo",
                                 subtitle = countupOutput("maximo"),
                                 color = "gray",
                                 icon = icon("arrow-up")
                               )
                        ),
                        column(width = 4,
                               bs4ValueBox(
                                 width = 12,
                                 value = "Promedio",
                                 subtitle = countupOutput("promedio"),
                                 color = "gray",
                                 icon = icon("percent")
                               )
                        )
                      ),
                      uiOutput("graficos")
                  ),
             tabPanel("Componentes",
                      column(width = 12, align ="center",
                             pickerInput("comps_sel", "Componentes a visualizar", choices = unique(comps_long$Componente),
                                         selected = unique(comps_long$Componente), multiple = T
                                         )),
                      uiOutput("graficos_componentes")
                      ),
             tabPanel("Pronóstico",
                      fluidRow(
                        column(width = 4, align ="center",
                               pickerInput("inf_tipo", "Tipo de inflación a pronosticar", choices = c("Subyacente", "No subyacente")
                               )),
                        column(width = 4, align = "center", sliderInput("periodos", "Periodos a pronosticar", value = 12,
                                                                        min = 1, max = 40
                        )),
                        column(width = 4, align = "center", 
                               awesomeRadio("modelo", "Modelo a utilizar", choices = c("ARIMA", "BATS"), inline = T)
                               )
                      ),
                      fluidRow(
                        column(width = 4,
                               reactableOutput("tabla_datos")
                               ),
                        column(width = 8,
                               column(width = 12,
                                      echarts4rOutput("pronostico"))
                               )
                      )
             )
           )
        )
  )
)
