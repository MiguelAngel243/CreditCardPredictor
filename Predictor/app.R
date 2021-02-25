if(interactive()){
    library(shiny)
    library(shinydashboard)
    library(shinyMatrix)
    library(shinyjs)
    library(plotly)
    library(cutpointr)
    load("objects")
    
    m <- matrix(rep(0,18),6,3, dimnames = list(paste(1:6,"° mes anterior"),c("Pagos Pendientes","Saldo","Amortizaciones")))
    lowrisk <- 0.15
    highrisk <- 0.2
    
    jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"  
    plotgraphs <- function(input,output){
        a<- unlist(head(rocdata[rocdata[,1]==lowrisk,],1))
        b<- unlist(head(rocdata[rocdata[,1]==highrisk,],1))
        c<- unlist(head(prdata[prdata[,1]==lowrisk,],1))
        d<- unlist(head(prdata[prdata[,1]==highrisk,],1))
        
        output$rocplot <- renderPlotly(
            plot_ly(rocdata, x=~V1, y=~V2, type="scatter", mode="lines",
                    hovertemplate=paste("Cutpoint: ",rocdata$V3), 
                    color=I("black")) %>% 
                layout(xaxis=list(title="Falsa Alarma"),
                       yaxis=list(title="Sensitividad"),
                       title="Curva Receiving-Operating",
                       showlegend=FALSE
                       ) %>% 
                add_segments(x=a[2], xend=a[2], y=0, yend=1, 
                             line=list(dash="dash", color="#FF8000")) %>% 
                add_segments(x=0, xend=1, y=a[3], yend=a[3], 
                             line=list(dash="dash", color="#FF8000")) %>% 
                add_segments(x=b[2], xend=b[2], y=0, yend=1,
                             line=list(dash="dash", color="#8B0000")) %>% 
                add_segments(x=0, xend=1, y=b[3], yend=b[3],
                             line=list(dash="dash", color="#8B0000")) 
        )
        
        output$prplot <- renderPlotly(
            plot_ly(prdata, x=~V1, y=~V2, type="scatter", mode="lines",
                    hovertemplate=paste("Cutpoint: ",prdata$V3),
                    color=I("black")) %>% 
                layout(xaxis=list(title="Sensitividad"),
                       yaxis=list(title="Valor Predictivo Positivo"),
                       title="Curva Precision-Recall", 
                       showlegend=FALSE) %>% 
                add_segments(x=c[2], xend=c[2], y=0, yend=1, 
                             line=list(dash="dash", color="#FF8000")) %>% 
                add_segments(x=0, xend=1, y=c[3], yend=c[3],
                             line=list(dash="dash", color="#FF8000")) %>% 
                add_segments(x=d[2], xend=d[2], y=0, yend=1,
                             line=list(dash="dash", color="#8B0000")) %>% 
                add_segments(x=0, xend=1, y=d[3], yend=d[3],
                             line=list(dash="dash", color="#8B0000")) 
        )
    }

ui <- dashboardPage(
    dashboardHeader(title="Predictor de Impago en Tarjetas de Crédito"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Predecir",tabName = "Predecir",icon=icon("calculator")),
            menuItem("Configurar",tabName="Configurar",icon=icon("sliders-h"))
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
                                tabPanel("Por Cliente", icon=icon("user-check"),
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
                                         actionButton("calc","Calcular",icon=icon("calculator")),
                                         actionButton("sample","Demostración",icon=icon("swatchbook")),
                                         textOutput("esperado")
                                ),
                                tabPanel("Por Lote", icon=icon("file-csv"),
                                         fileInput("filesel","Archivo de Datos", 
                                                   buttonLabel = "Seleccionar",accept = ".csv" ),
                                         downloadButton("calcfile","Procesar y Descargar"), br(), br(),
                                         "Carge un archivo csv con los datos de los clientes. De click en 
                                         procesar, espere unos segundos y obtendrá el archivo con una nueva columna 
                                         indicando la categoría de riesgo adjudicada por cada cliente."),
                                height=650,
                                width=19
                            )
                        ),
                        box(title="Resultados", width=19,
                            infoBoxOutput("prob"),
                            infoBoxOutput("ppv"),
                            actionButton("return","Realizar nuevo calculo",icon=icon("calculator"))
                        )
                    )
            ),
            tabItem(tabName = "Configurar",
                    fluidRow(
                        tabBox(id="conftab",title="Umbrales",
                               tabPanel("Por Relación de Costos",
                                        numericInput("rel","Relación de Costo FP/FN:",0.3,step=0.1),
                                        "Introduzca el cociente entre el costo de brindarle atención a un cliente que sí 
                                        cumplirá, entre el costo de no brindarle atención a un cliente que sí incumplirá."),
                               tabPanel("Por Valores Predictivos",
                                        numericInput("vppl","Valor Predictivo Positivo, Grado Medio",value=0.3, step=0.1),
                                        numericInput("vppm","Valor Predictivo Positivo, Grado Alto",value=0.5, step=0.1),
                                        "Introduzca el porcentaje de confianza de considerar positivo a un cliente para 
                                        los intervalos de riesgo bajo-medio y medio-alto."
                               )
                        ),
                        actionButton("calccut","Establecer",icon=icon("cogs"))
                    ),
                    fluidRow(
                        column(5,
                               plotlyOutput("rocplot",width="100%")),
                        column(5,plotlyOutput("prplot",width="100%")))
            )
            
        )
        
        
    )
)

server <- function(input, output, session) { 

    plotgraphs(input,output)
    
    observeEvent(input$calc,{
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
        p <- round(predict(finalmodel,clientdata,type="prob")$Si,2)
        print(lowrisk)
        if(p<lowrisk){
            colr<-"green"
            riesgo <- "Riesgo Bajo"
        }
        if(p>=lowrisk & p<highrisk){
            colr<-"yellow"
            riesgo <- "Riesgo Medio"
        }
        if(p>=highrisk){
            colr<-"red"
            riesgo <- "Riesgo Alto"
        }
        ppvv <- round(prdata[prdata[,1]==p,]$V2,2)*100
        
        output$prob <- renderInfoBox({
            infoBox("Categoría",value=riesgo,fill=TRUE,color=colr,icon=icon("hand-holding-usd"))
        })
        
        output$ppv <- renderInfoBox({
            infoBox("Valor Positivo Predictivo",value=paste(ppvv,"%"),fill=FALSE,color="black", icon=icon("exclamation-triangle"))
        })
    })
    
    observeEvent(input$sample,{
        ran  <- round(runif(1,1,20))
        shiny::updateSelectInput(session,"gensel",selected=demos[ran,2])
        shiny::updateSelectInput(session,"educsel",selected=demos[ran,3])
        shiny::updateSelectInput(session,"marsel",selected=demos[ran,4])
        shiny::updateNumericInput(session,"edad",value=demos[ran,5])
        shiny::updateNumericInput(session,"limitbal",value=demos[ran,1])
        m <- matrix(demos[ran,6:23],6,3, dimnames = list(paste(1:6,"° mes anterior"),c("Pagos Pendientes","Saldo","Amortizaciones")))
        updateMatrixInput(session,"financ",value=m)
        output$esperado <- renderText(
            if(demos[ran,24]=="Si"){
                "Este usuario demostrado: Realmente incumplió con el siguiente pago"
            }else{
                "Este usuario demostrado: Realmente cumplió con el siguiente pago"
            }
            )
    })
    
    output$calcfile <- downloadHandler(
        filename = "riesgo_impago_resultados.csv", 
        content = function(file){
            filein <- input$filesel
            data <- read.csv(filein$datapath)
            data$SEX <- factor(ifelse(data$SEX==1,"Hombre","Mujer"))
            data$EDUCATION <- factor(mapvalues(data$EDUCATION,0:6,c("Otro","Maestría/Doctorado","Licenciatura","Bachillerato","Otro","Otro","Otro")))
            data$MARRIAGE <- factor(mapvalues(data$MARRIAGE, 0:3, c("Otro","Casado","Soltero","Otro")))
            
            colnames(data)[25] <- "default"
            colnames(data)[7] <- "PAY_1"
            
            data <- mutate(data,INCRPAYS=PAY_AMT1-PAY_AMT6)
            data <- mutate(data,INCRBILL=(BILL_AMT1-BILL_AMT6))
            
            for(i in 1:nrow(data)){
                data[i,28] <- sum(data[i,19:24]==0)
            }
            
            for(i in 1:nrow(data)){
                data[i,29] <- sum(data[i,7:12]>0)
            }
            
            colnames(data)[28] <- "NNULLPAYMENTS"
            colnames(data)[29] <- "IRREGMONTHS"
            
            data[30] <- predict(finalmodel,data,type="prob")[,2] %>%
                cut(breaks=c(0,lowrisk,highrisk,1),labels=c("Bajo","Medio","Alto"))
            
            colnames(data)[30] <- "RIESGO CALCULADO"
            write.csv(data,file)
        }
    )
    
    observeEvent(input$return,{
        js$collapse("d")
    })
    
    observeEvent(input$calccut,{
        if(input$conftab=="Por Relación de Costos"){
            lowrisk <<- round(cutpointr(x=finalmodel$finalModel$fitted.values,
                                 class=demos$default, 
                                 method=minimize_metric, 
                                 metric = misclassification_cost,
                                 cost_fp=input$rel, cost_fn=1)$optimal_cutpoint,2)
            highrisk <<- lowrisk
        }
        if(input$conftab=="Por Valores Predictivos"){
            lowrisk <<- round(cutpointr(x=finalmodel$finalModel$fitted.values,
                                 class=demos$default, 
                                 method=maximize_metric, 
                                 metric=metric_constrain, 
                                 constrain_metric=ppv, 
                                 min_constrain=input$vppl)$optimal_cutpoint,2)
            
            highrisk <<- round(cutpointr(x=finalmodel$finalModel$fitted.values,
                                  class=demos$default, 
                                  method=maximize_metric, 
                                  metric=metric_constrain, 
                                  constrain_metric=ppv, 
                                  min_constrain=input$vppm)$optimal_cutpoint,2)
        }
        
        plotgraphs(input,output)
    })
}

shinyApp(ui, server)

}