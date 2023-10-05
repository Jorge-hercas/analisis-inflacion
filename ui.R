

fluidPage(
  gfonts::use_pkg_gfont("oswald"),
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
           img(src = "https://vfsgroup.carrd.co/assets/images/image01.png?v=392d0626",height="50px")),
    column(width = 4, 
           h1("INFLACIÓN EN MÉXICO. ", id = "titlepanel"),
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
    downloadButton("download", "Descargar datos", icon = icon("file-excel"))
    ),
    column(width = 9,
           tabsetPanel(
             tabPanel("Subyacente vs no subyacente",
                      uiOutput("graficos")
                  ),
             tabPanel("Componentes")
           )
        )
  )
)
