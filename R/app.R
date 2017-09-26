#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(forecast)

# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel("Cargar archivo"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      fileInput('TextFile', 'Seleccione el archivo a cargar',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain'
                )
      ),
      
      tags$hr(),
      
      radioButtons('skipper', 'Registros a saltar',
                   c(Zero=0,
                     One=1
                   )),
      tags$hr(),

      numericInput('year','Ingrese año de inicio',value =2017 ),
      tags$hr(),
      numericInput('month','Ingrese mes de inicio',value=01),
      tags$hr(),
      numericInput('frequency','Ingrese la frecuencia',value=12)
    ),

    mainPanel(

      h4("Gráfico de la serie de tiempo"),
      plotOutput("contents1"),

      #Horizontal line ----
      tags$hr(),

      h4("Resúmen de los datos"),
      verbatimTextOutput("contents2"),
      
      #Horizontal line ----
      tags$hr(),

      h4("Determinar serie Aditiva o Multiplicativa"),
      plotOutput("logdata"),
      
      tags$hr(),
      
      h4("Autocorrelacion PAC Datos"),
      plotOutput("pac_Data"),
      
      tags$hr(),
      h4("Autocorrelacion parcial PACF Datos"),
      plotOutput("pacf_Data"),
      
      tags$hr(),
      
      h4("Autocorrelacion PAC logData"),
      plotOutput("pacLogData"),
      
      tags$hr(),
      
      h4("Autocorrelacion parcial PACF logData"),
      plotOutput("pacfLogData"),
      
      tags$hr(),
      
      h4("Estacionalidad"),
      plotOutput("estacionalidad"),
      
      tags$hr(),
      
      h4("Descomposición multiplicativa de la serie original"),
      plotOutput("descomp_mult_data"),
      
      tags$hr(),
      
      h4("Descomposición aditiva del logaritmo"),
      plotOutput("descomp_adit_Log"),
      
      tags$hr(),
      
      h4("Descomposición aditiva de la serie"),
      plotOutput("descomp_adit_data"),
      
      tags$hr(),
      
    
      h4("Tendencia estimada por descomposición clásica"),
      plotOutput("tendencia_desc_data"),
      
      h4("Tendencia estimada por descomposición clásica (Log)"),
      plotOutput("tendencia_desc_logdata"),
      
      h4("Error de los datosy logdatos ADITIVA"),
      plotOutput("error_datos_logdatos"),
      
      h4("Error de los datosy logdatos MULTIPLICATIVA"),
      plotOutput("error_datos_logdatos_multi")
      
      )))

# Define server logic to read selected file ----
server <- function(input, output) {

  data_l<-reactive({
    inFile <- input$TextFile
    if (is.null(inFile))
      return(NULL)
    
    data_st<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
             frequency = input$frequency)
    return(data_st)
  })

    #GRAFICAR LA SERIE DE TIEMPO
    output$contents1 <- renderPlot({
    data_st<-data_l()
    plot.ts(data_st, ylim=c(min(data_st),max(data_st)), type = 'o', lwd = 2)
    grid()

  })

    #ADICIONAR EL RESUMEN DE LOS DATOS
    output$contents2 <- renderPrint({
    data_st<-data_l()
    summary(data_st)})
    
    #GRAFICAR LOGDATA PARA DETERMINAR MOD ADITIVO - MULTIPLICATIVO
    output$logdata <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      plot(logdata,ylim=c(min(logdata),max(logdata)))
      title(main="log(data)")
    })
    
    #GRAFICAR AUTOCORRELACIÓN DATOS 
    output$pac_Data <- renderPlot({
      data_st <- data_l()
      acf(data_st)
    })
    
    #GRAFICAR AUTOCORRELACIÓN PARCIAL DATOS  
    output$pacf_Data <- renderPlot({
      data_st <- data_l()
      pacf(data_st)
    })

    #GRAFICAR PAC LOGDATOS
    output$pacLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      acf(logdata)
      
    })
    
    #GRAFICAR PACF LOGDATOS
    output$pacfLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      pacf(logdata)
    })
    
    #GRAFICAR ESTACIONALIDAD
    output$estacionalidad <- renderPlot({
      data_st <- data_l()
      boxplot(data_st~cycle(data_st),names=month.abb)
      title(main="Estacionalidad")
    })
    
    #GRAFICAR DESCOMPOSICIÓN MULTIPLICATIVA DE LA SERIE
    output$descomp_mult_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type = "multiplicative"))
    })
    
    #GRAFICAR DESCOMPOSICIÓN ADITIVA DEL LOGARITMO
    output$descomp_adit_Log <- renderPlot({
      data_st <- data_l()
      plot(decompose(log(data_st), type="additive"))
    })
    
    #GRAFICAR DESCOMPOSICIÓN ADITIVA DE LA SERIR
    output$descomp_adit_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type="additive"))
    })
    
    #GRAFICAR TENDENCIA POR DESCOMPOSIÓN DE LA SERIE
    output$tendencia_desc_data <- renderPlot({
      data_st <- data_l()
      Tt=decompose(data_st,type="additive")$trend
      plot(Tt,ylim=c(min(data_st),max(data_st)),main="Componente tendencia estimada por descomposición
     clásica\ gráfico ajustado a rango de variación de la serie")
    })
    
    #GRAFICAR TENDENCIA POR DESCOMPOSIÓN DEL LOGARITMO DE LA SERIE
    output$tendencia_desc_logdata <- renderPlot({
      data_st <- data_l()
      log_data <- (data_st)
      Tt_log=decompose(log_data,type="multiplicative")$trend
      plot(Tt_log,ylim=c(min(data_st),max(data_st)),main="Componente tendencia estimada por descomposición
           clásica\ gráfico ajustado a rango de variación del log de la serie")
    })
    
      #GRAFICAR ERROR DATOS Y LOGDATOS ADITIVA
      output$error_datos_logdatos <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        Tt_error_datos=decompose(data_st,type="additive")$random
        plot(Tt_error_datos,main="Componente error estimada por descomposición clásica aditiva")
      
    })
      
      #GRAFICAR ERROR DATOS Y LOGDATOS MULTIPLICATIVA
      output$error_datos_logdatos_multi <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        error_datos_logdatos_multi=decompose(data_st,type="multiplicative")$random
        plot(error_datos_logdatos_multi,main="Componente error estimada por descomposiciÓn clásica multiplicativa")
        
      })
}
    

# Create Shiny app ----
shinyApp(ui, server)
