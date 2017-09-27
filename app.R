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
library(descomponer)
library(shinythemes)
library(taRifx)
library(TSA)
library(msir)
library(fANCOVA)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("superhero"),

  navbarPage("Análisis serie de tiempo",
             tabPanel("Análisis descriptivo",
             
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
                        'text/plain')),
      
              tags$hr(),
      
              radioButtons('skipper', 'Registros a saltar',
                   c(Zero=0,
                     One=1
                   )),
              tags$hr(),

              numericInput('year','Ingrese aÃ±o de inicio',value =2017 ),
              tags$hr(),
              numericInput('month','Ingrese mes de inicio',value=01),
              tags$hr(),
              numericInput('frequency','Ingrese la frecuencia',value=12)
              ),#parentesis sidebarPanel
              

              mainPanel(
                tabsetPanel(
                  
                  tabPanel("Análisis básico ST",
                          h4("Gráfico de la serie de tiempo"),
                          plotOutput("contents1"),

                          #Horizontal line ----
                          tags$hr(),

                          h4("Resúmen de los datos"),
                          verbatimTextOutput("contents2"),
      
                          #Horizontal line ----
                          tags$hr(),

                          h4("Determinar serie Aditiva o Multiplicativa"),
                          plotOutput("logdata")
                          ),#paréntesis Análisis básico de la serie
                  
                  tabPanel("Autocorrelación",
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
                          plotOutput("pacfLogData")
                          ),#paréntesis tabpanel Autocorrelación
                  
                  tabPanel("Estacionalidad - Descomp",
                          h4("Estacionalidad"),
                          plotOutput("estacionalidad"),
      
                          tags$hr(),
      
                          h4("DescomposiciÃ³n multiplicativa de la serie original"),
                          plotOutput("descomp_mult_data"),
      
                          tags$hr(),
      
                          h4("DescomposiciÃ³n aditiva del logaritmo"),
                          plotOutput("descomp_adit_Log"),
      
                          tags$hr(),
      
                          h4("DescomposiciÃ³n aditiva de la serie"),
                          plotOutput("descomp_adit_data")
                
                          ),#paréntesis tabpanel estacionalidad y descomposición
      
            
                  tabPanel("Tendencia - Error",
    
                        h4("Tendencia estimada por descomposiciÃ³n clÃ¡sica"),
                        plotOutput("tendencia_desc_data"),
      
                        tags$hr(),
      
                        h4("Tendencia estimada por descomposiciÃ³n clÃ¡sica (Log)"),
                        plotOutput("tendencia_desc_logdata"),
      
                        tags$hr(),
      
                        h4("Error de los datosy logdatos ADITIVA"),
                        plotOutput("error_datos_logdatos"),
      
                        tags$hr(),
      
                        h4("Error de los datosy logdatos MULTIPLICATIVA"),
                        plotOutput("error_datos_logdatos_multi"),
      
                        tags$hr(),
      
                        h4("Periodograma sobre los logaritmos diferenciados"),
                        plotOutput("periodograma_log")
              
                      )#paréntesis tabpanel Tendencia error
            )#paréntesis del tapsetpanel
          )#paréntesis main panel
        )#paréntesis sidebarlayout
      ),#paréntesis tabpanel
      
      tabPanel("MODELO 1",
               
               titlePanel("Análisis Modelo 1"),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Análisis básico ST",
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            verbatimTextOutput("st_ajustado_original"),
                            
                            tags$hr(),
                            
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            plotOutput("ythat1_ajustado_original"),
                            
                            tags$hr(),
                            
                            h4("Gráfico de residuales en escala LOG con límites 2 sigma"),
                            plotOutput("residuales2sigma"),
                            
                            tags$hr(),
                            
                            h4("Pronosticando n periodos en escala original"),
                            verbatimTextOutput("pronostico_periodos"),
                            
                            tags$hr(),
                            
                            h4("Serie de tiempo con pronosticos puntuales"),
                            plotOutput("pronostico_puntuales")
                  
                   )#paréntesis tabPanel interior
                 )#paréntesis tabsetPanel Modelo 1
               )#paréntesis mainPanel Modelo 1
      )#paréntesis tabPanel Modelo1
    )#Paréntesis NavbarPage
  )#paréntesis fuidPage

# Define server logic to read selected file ----
server <- function(input, output) {

  data_l<-reactive({
    inFile <- input$TextFile
    if (is.null(inFile))
      return(NULL)
    
    data_st<-ts(scan(inFile$datapath,skip=input$skipper),start=c(input$year,input$month),
             frequency = input$frequency)
    return(data_st)
  })#anterior

    #GRAFICAR LA SERIE DE TIEMPO
    output$contents1 <- renderPlot({
    data_st<-data_l()
    plot.ts(data_st, ylim=c(min(data_st),max(data_st)), type = 'o', lwd = 2)
    grid()

  })#anterior

    #ADICIONAR EL RESUMEN DE LOS DATOS
    output$contents2 <- renderPrint({
    data_st<-data_l()
    summary(data_st)})#anterior
    
    #GRAFICAR LOGDATA PARA DETERMINAR MOD ADITIVO - MULTIPLICATIVO
    output$logdata <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      plot(logdata,ylim=c(min(logdata),max(logdata)))
      title(main="log(data)")
    })#anterior
    
    #GRAFICAR AUTOCORRELACION DATOS 
    output$pac_Data <- renderPlot({
      data_st <- data_l()
      acf(data_st)
    })#anterior
    
    #GRAFICAR AUTOCORRELACION PARCIAL DATOS  
    output$pacf_Data <- renderPlot({
      data_st <- data_l()
      pacf(data_st)
    })#anterior

    #GRAFICAR PAC LOGDATOS
    output$pacLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      acf(logdata)
      
    })#anterior
    
    #GRAFICAR PACF LOGDATOS
    output$pacfLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      pacf(logdata)
    })#anterior
    
    #GRAFICAR ESTACIONALIDAD
    output$estacionalidad <- renderPlot({
      data_st <- data_l()
      boxplot(data_st~cycle(data_st),names=month.abb)
      title(main="Estacionalidad")
    })#anterior
    
    #GRAFICAR DESCOMPOSICION MULTIPLICATIVA DE LA SERIE
    output$descomp_mult_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type = "multiplicative"))
    })#anterior
    
    #GRAFICAR DESCOMPOSICION ADITIVA DEL LOGARITMO
    output$descomp_adit_Log <- renderPlot({
      data_st <- data_l()
      plot(decompose(log(data_st), type="additive"))
    })#anterior
    
    #GRAFICAR DESCOMPOSICION ADITIVA DE LA SERIR
    output$descomp_adit_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type="additive"))
    })#anterior
    
    #GRAFICAR TENDENCIA POR DESCOMPOSION DE LA SERIE
    output$tendencia_desc_data <- renderPlot({
      data_st <- data_l()
      Tt=decompose(data_st,type="additive")$trend
      plot(Tt,ylim=c(min(data_st),max(data_st)),main="Componente tendencia estimada por descomposiciÃ³n
     clÃ¡sica\ grÃ¡fico ajustado a rango de variaciÃ³n de la serie")
    })#anterior
    
    #GRAFICAR TENDENCIA POR DESCOMPOSION DEL LOGARITMO DE LA SERIE
    output$tendencia_desc_logdata <- renderPlot({
      data_st <- data_l()
      log_data <- (data_st)
      Tt_log=decompose(log_data,type="multiplicative")$trend
      plot(Tt_log,ylim=c(min(data_st),max(data_st)),main="Componente tendencia estimada por descomposiciÃ³n
           clÃ¡sica\ grÃ¡fico ajustado a rango de variaciÃ³n del log de la serie")
    })#anterior
    
      #GRAFICAR ERROR DATOS Y LOGDATOS ADITIVA
      output$error_datos_logdatos <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        Tt_error_datos=decompose(data_st,type="additive")$random
        plot(Tt_error_datos,main="Componente error estimada por descomposiciÃ³n clÃ¡sica aditiva")
      
    })#anterior
      
      #GRAFICAR ERROR DATOS Y LOGDATOS MULTIPLICATIVA
      output$error_datos_logdatos_multi <- renderPlot({
        data_st <- data_l()
        log_data <- log(data_st)
        error_datos_logdatos_multi=decompose(data_st,type="multiplicative")$random
        plot(error_datos_logdatos_multi,main="Componente error estimada por descomposiciÃn clÃ¡sica multiplicativa")
        
      })#anterior
      
      #GRAFICAR PERIODODOGRAMA
      output$periodograma_log <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        periodogram(diff(log(data_st))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        
      })
      
      #GRAFICAR SERIE DE TIEMPO DE VALORES AJUSTADOS EN ESCALA ORIGINAL
      output$st_ajustado_original <- renderPrint({
        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #´indice de tiempo
        yt2=ts(data_st[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))
        #trimestre=relevel(trimestre,ref="4Q")
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
      })#anterior
      
      #GRAFICAR SERIE DE TIEMPO DE VALORES AJUSTADOS EN ESCALA ORIGINAL
      output$ythat1_ajustado_original <- renderPlot({
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(input$year,input$month))
        plot.ts(ythat1)
      })
      
      #GRÁFICO DE RESIDUALES EN ESCALA LOG CON LIMITES 2sigma
      output$residuales2sigma <- renderPlot({
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\nModelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
      })
      
      ##PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL
      output$pronostico_periodos <- renderPrint({
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        trimestre.nuevo1 <- as.numeric(substr(trimestre.nuevo[1], start = 1,stop = 1))
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        plot.ts(ytf)
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo1),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        predicciones1
      })
      
      
      #GRÁFICO DE SERIE DE TIEMPO CON PRONOSTICOS PUNTUALES
      output$pronostico_puntuales <- renderPlot({
        ytpron1=ts(predicciones1[,1],frequency=4,start=c(2016,4)) #los pron´osticos comienzan desde 1993-Q4
        accuracy(ytpron1,ytf) #Calculando exactitud de los pron´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRON´OSTICOS
        plot(yt,main="Serie real, ajustes y pron´osticos\nModelo Log cuadr´atico estacional")
        lines(ythat1,col=2)
        lines(ytpron1,col=4)
        legend("topleft",legend=c("Original","Ajustada","Pron´osticos"),col=c(1,2,4),lty=1)
      })
      
}#paréntesis para cerrar el server
    

# Create Shiny app ----
shinyApp(ui, server)
