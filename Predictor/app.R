if(interactive()){
    library(shiny)
    library(shinydashboard)
    library(shinyMatrix)
    library(shinyjs)
    library(plotly)
    
    m <- matrix(rep(0,18),6,3, dimnames = list(c(seq(1:6)),c("Pagos Pendientes","Estado de Cuenta","Amortizaciones")))
    
    jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"

ui <- dashboardPage(
    dashboardHeader(title="Predictor de Impago en Tarjetas de Crédito"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Predecir",tabName = "Predecir",icon=icon("dashboard")),
            menuItem("Configurar",tabName="Configurar",icon=icon("th"))
        )
    ),
    dashboardBody(
        useShinyjs(),
        extendShinyjs(text = jscode, functions=c("collapse")),
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
                                             selectInput("marsel","Estado Marital",c("Soltero","Casado","Otro")),
                                             numericInput("edad","Edad",value=0)
                                         ),box(title="Datos Financieros",
                                               numericInput("limitbal","Límite de Crédito",0),
                                               "Ingrese los siguientes datos mensuales, comenzando por el mes pasado (1):",
                                               matrixInput("financ",value = m, class = "numeric", cols = list(
                                                   names = TRUE
                                               ),
                                               rows = list(
                                                   names = TRUE
                                               ))
                                               
                                         ),
                                         actionButton("calc","Calcular"),
                                         actionButton("sample","Demostración"),
                                ),
                                tabPanel("Por Lote",
                                         fileInput("filesel","Archivo de Datos", buttonLabel = "Seleccionar"),
                                         actionButton("calcfile","Calcular")),
                                         
                                height=650,
                                width=19
                            )
                        ),
                        box(title="Resultados", width=19,
                            infoBoxOutput("prob")
                        )
                    )
            ),
            tabItem(tabName = "Configurar",
                    fluidRow(
                        tabBox(id="conftab",title="Umbrales",
                               tabPanel("Por Relación de Costos",
                                        numericInput("rel","Relación de Costo FP/FN:",3)),
                               tabPanel("Por Valores Predictivos",
                                        numericInput("vpp","Valor Predictivo Positivo",value=0),
                                        numericInput("vpn","Valor Predictivo Negativo",value=0)
                                       )
                        )
                        ),
                    plotlyOutput("rocplot",width="50%")
                    )
                    
        )
        
        
    )
)

server <- function(input, output, session) { 
    output$rocplot <- renderPlotly(
        plot_ly(rd,x=~fa,y=~se,type="scatter",mode="lines",hovertemplate=paste("Cutpoint: ",rd$cp)) %>% layout(xaxis=list(title="Falsa Alarma"),yaxis=list(title="Sensitividad"))
        )
    
    observeEvent(input$calc,{
        output$prob <- renderInfoBox({
            js$collapse("d")
            clientdata <- data.frame(
                input$limitbal,
                input$gensel,
                input$educsel,
                input$marsel,
                input$edad,
                input$financ[1,1],
                input$financ[1,2],
                input$financ[1,3],
                input$financ[1,2]-input$financ[6,2],
                input$financ[1,3]-input$financ[6,3],
                sum(input$financ[,3]==0),
                sum(input$financ[,1]>0)
            )
            colnames(clientdata) <- c("LIMIT_BAL","SEX","EDUCATION","MARRIAGE","AGE","PAY_1",
                                     "BILL_AMT1","PAY_AMT1","INCRBILL","INCRPAYS","NNULLPAYMENTS",
                                     "IRREGMONTHS")
            str(clientdata)
            p <- predict(model8,clientdata,type="prob")$Si*100
            if(p<20){colr<-"green"}
            if(p>=20 & p<30){colr<-"yellow"}
            if(p>=30){colr<-"red"}
            infoBox("Probabilidad",value=p,fill=TRUE,color=colr)
        })
    })
    
    observeEvent(input$sample,{
        ran  <- round(runif(1,1,20))
        shiny::updateSelectInput(session,"gensel",selected=demos[ran,2])
        shiny::updateSelectInput(session,"educsel",selected=demos[ran,3])
        shiny::updateSelectInput(session,"marsel",selected=demos[ran,4])
        shiny::updateNumericInput(session,"edad",value=demos[ran,5])
        shiny::updateNumericInput(session,"limitbal",value=demos[ran,1])
        m <- matrix(demos[ran,6:23],6,3, dimnames = list(c(seq(1:6)),c("Pagos Pendientes","Estado de Cuenta","Amortizaciones")))
        updateMatrixInput(session,"financ",value=m)
    })
    
    observeEvent(input$calcfile,{
        file <- input$filesel
        data <- read.csv(file$datapath)
        data <- mutate(data,INCRPAYS=PAY_AMT1-PAY_AMT6)
        data <- mutate(data,INCRBILL=(BILL_AMT1-BILL_AMT6))
    })
}

shinyApp(ui, server)

}