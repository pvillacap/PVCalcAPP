###############################################################################################
## APLICACION R-SHINY: 
##    Lee un fichero Excel y crea un informe de comparabilidad de metodos de ensayo clinico:
##      1- 
##      2- Una tabla con todos los datos del fichero
###############################################################################################

rm(list=ls())
gc()

require("shiny")
require("shinydashboard")
require("shinyjs")
require("openxlsx")
require("DT")
library("plotly")

# Refactorización del código para mejorar la legibilidad y organizaciÃ³n

# Definición de estilos CSS
custom_css <- tags$head(
  tags$style(HTML(
    "h4{margin: 2%;}
    h3{text-align: center}
    .progress-bar{ 
    background-color: #605CA8;
    }
    #informe{margin: 4%;
    width:272px;}
    "
  ))
)

# Panel de parÃ¡metros para Bland-Altman
bland_altman_panel <- tabPanel(
  "Bland - Altman",
  h4("Parámetros Bland-Altman"),
  numericInput("alphaba", "alpha", min = 0, max = 1, value = 0.05),
  h4("Parámetros histograma"),
  numericInput("bins", "Nº Barras", min = 0, max = 30, step = 1, value = 10),
  h4("Parámetros informe Bland Altman"),
  selectInput("acuerdo", "Tipo acuerdo", c("Proporciones", "Absoluto")),
  numericInput("loa", "Limite de acuerdo", min = 0, max = 1000, step = 1, value = 10)
 # selectInput("tipoError", "Tipo error", c("Porcentaje (%)", "Absoluto")),
 # numericInput("bias", "Limite de Error", min = 0, max = 1000, step = 1, value = 10)
)

# Panel de parÃ¡metros para Passing-Bablok
passing_bablok_panel <- tabPanel(
  "Passing_Babblok",
  h4("ParÃ¡metros Passing Bablok"),
  numericInput("alphapb", "alpha", min = 0, max = 1, value = 0.05),
  selectInput("slope_measurepb", "Slope Meassure", c("radian", "tangent")),
  h4("Parámetros criterio de aceptación"),
  numericInput("criterio", "Criterio aceptacion(%)", min = 0, max = 100, value = 10)
)

# Panel de parÃ¡metros para Deming
deming_panel <- tabPanel(
  "Deming",
  h4("ParÃ¡metros Deming"),
  numericInput("alphadm", "alpha", min = 0, max = 1, value = 0.05),
  numericInput("errorRatio", "Error Ratio", min = 0, max = 100, value = 1)
)

# TabBox para los métodos
metodos_tabbox <- tabBox(
  id = "metodo",
  height = "750px",
  width = "12",
  tabPanel("Bland Altman", 
           fluidRow(
             column(12, plotlyOutput("BA"))
           ),
           hr(),
           fluidRow(
             column(6, plotlyOutput("hist")),
             column(6, plotlyOutput("qqnorm"))
           ),
           hr(),
           uiOutput("BA_h3_title"), # Aquí se renderizarán los H3 condicionalmente,
           br(),
           DT::dataTableOutput("BAshapiro"),
           hr(),
           uiOutput("BAstats_title"),
           DT::dataTableOutput("BAstats"),
           uiOutput("BAtabla"),
           
           DT::dataTableOutput("BAtable"),
    
  ),
  tabPanel("Passing_Bablok", 
           fluidRow(
             column(6, plotlyOutput("PB")),
             column(6, plotlyOutput("resPb"))
           ),
           hr(),
           h3("Resultados Regresión Passing Bablok"),
           DT::dataTableOutput("PBtable")
  ),
  tabPanel("Deming", 
           fluidRow(
             column(6, plotlyOutput("DM")),
             column(6, plotlyOutput("resDM"))
           ),
           hr(),
           h3("Resultados Regresión Deming"),
           DT::dataTableOutput("DMtable", width = '100%')
  )
)

# Definición de la interfaz de usuario
ui <- dashboardPage(
  dashboardHeader(
    title = "Comparación de Métodos",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 350,
    custom_css,
    br(),
    fileInput("mi_fichero", "Fichero Excel a procesar:",
              accept = c("application/vnd.ms-excel",
                         "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
    ),
    tabsetPanel(
      id = "params",
      bland_altman_panel,
      passing_bablok_panel,
      deming_panel
    ),
    br(),
    br(),
    shinyjs::useShinyjs(),
    shinyjs::disabled(actionButton(inputId = "procesar", "PROCESAR", width = 270)),
    downloadButton("informe", "Generar informe", width = 270)
  ),
  dashboardBody(
    fluidRow(
      metodos_tabbox
    )
  ),
  skin = "purple"
)


