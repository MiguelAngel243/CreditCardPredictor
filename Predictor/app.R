library(shiny)
library(shinydashboard)
library(shinyMatrix)

m <- matrix(rep(0,18),6,3, dimnames = list(c(seq(1:6)),c("Pagos Pendientes","Estado de Cuenta","Amortizaciones")))

ui <- dashboardPage(
    dashboardHeader(title="Predictor de Impago en Tarjetas de Crédito"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Predecir",tabName = "Predecir",icon=icon("dashboard")),
            menuItem("Configurar",tabName="Configurar",icon=icon("th"))
        )
    ),
    dashboardBody(
            tabItems(
                tabItem(tabName = "Predecir",
                        fluidRow(
                        box(id="d", title="Ingreso de Datos",collapsible = TRUE, width = 19,
                        tabBox(
                            id="tabset1",
                            tabPanel("Por Cliente",
                                     box(title="Datos Demográficos",
                                         selectInput("gensel","Género",c("Hombre","Mujer")),
                                         selectInput("educsel","Grado Educativo",c("Licenciatura","Maestría/Doctorado","Bachillerato","Otro")),
                                         selectInput("marsel","Estado Marital",c("Soltero","Casado","Otro"))
                                     ),box(title="Datos Financieros",
                                           numericInput("limitbal","Límite de Crédito",0),
                                           "Ingrese los siguientes datos mensuales, comenzando por el mes pasado (1):",
                                           matrixInput("financ",value = m,cols = list(
                                               names = TRUE
                                           ),
                                           rows = list(
                                               names = TRUE
                                           ))
                                           
                                     ),
                                     submitButton("Calcular")
                            ),
                            tabPanel("Por Lote",
                                     fileInput("filesel","Archivo de Datos", buttonLabel = "Seleccionar")),
                            height=650,
                            width=19
                        )
                        ),
                        box(title="Resultados", width=19)
                        )
                ),
                tabItem(tabName = "Configurar",
                        numericInput("rel","Relación de Costo FP/FN:",3))
            )
        
        
    )
)

server <- function(input, output) { 
    
    }

shinyApp(ui, server)
