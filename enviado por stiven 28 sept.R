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
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(theme = shinytheme("superhero"),

  navbarPage("Analisis serie de tiempo",
             tabPanel("Analisis descriptivo",
             
              # App title ----
              titlePanel("A N A L I - C"),

              # Sidebar layout with input and output definitions ----
              sidebarLayout(
  
              # Sidebar panel for inputs ----
              sidebarPanel(

              fileInput('TextFile', 'C A R G U E - D A T O S',
                        accept = c(
                        'text/csv',
                        'text/comma-separated-values',
                        'text/tab-separated-values',
                        'text/plain')),
      
              tags$hr(),
      
              radioButtons('skipper', 'R E G I S T R O S - A - S A L T A R',
                   c(ONE=1,
                     ZERO=0
                    
                   )),
              tags$hr(),

              numericInput('year','A N I O - I N I C I O',value =2017 ),
              tags$hr(),
              sliderInput('month','M E S - I N I C I O',value=01, min = 01,max = 12),
              tags$hr(),
              sliderInput('frequency','F R E C U E N C I A',value = 12,min=01,max=12)
              ),#parentesis sidebarPanel
              

              mainPanel(
                tabsetPanel(
                  
                  tabPanel("Analisis basico ST",
                          h4("Grafico de la serie de tiempo"),
                          plotOutput("contents1"),

                          #Horizontal line ----
                          tags$hr(),

                          h4("Resumen de los datos"),
                          verbatimTextOutput("contents2"),
      
                          #Horizontal line ----
                          tags$hr(),

                          h4("Determinar serie Aditiva o Multiplicativa"),
                          plotOutput("logdata")
                          ),#parÃ©ntesis AnÃ¡lisis bÃ¡sico de la serie
                  
                  tabPanel("Autocorrelacion",
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
                          ),#parÃ©ntesis tabpanel AutocorrelaciÃ³n
                  
                  tabPanel("Estacionalidad - Descomp",
                          h4("Estacionalidad"),
                          plotOutput("estacionalidad"),
      
                          tags$hr(),
      
                          h4("Descomposicion multiplicativa de la serie original"),
                          plotOutput("descomp_mult_data"),
      
                          tags$hr(),
      
                          h4("Descomposicion aditiva del logaritmo"),
                          plotOutput("descomp_adit_Log"),
      
                          tags$hr(),
      
                          h4("Descomposicion aditiva de la serie"),
                          plotOutput("descomp_adit_data")
                
                          ),#parÃ©ntesis tabpanel estacionalidad y descomposiciÃ³n
      
            
                  tabPanel("Tendencia - Error",
    
                        h4("Tendencia estimada por descomposicion clasica"),
                        plotOutput("tendencia_desc_data"),
      
                        tags$hr(),
      
                        h4("Tendencia estimada por descomposicion clasica (Log)"),
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
              
                      )#parÃ©ntesis tabpanel Tendencia error
            )#parÃ©ntesis del tapsetpanel
          )#parÃ©ntesis main panel
        )#parÃ©ntesis sidebarlayout
      ),#parÃ©ntesis tabpanel
      
      tabPanel("MODELO 1",
               
               titlePanel("Analisis Modelo 1"),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo 1",
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            verbatimTextOutput("st_ajustado_original"),
                            
                            tags$hr(),
                            
                            h4("Serie de tiempo de valores ajustados en escala original"),
                            plotOutput("ythat1_ajustado_original"),
                            
                            tags$hr(),
                            
                            h4("GrÃ¡fico de residuales en escala LOG con limites 2 sigma"),
                            plotOutput("residuales2sigma"),
                            
                            tags$hr(),
                            
                            h4("Pronosticando n periodos en escala original"),
                            tableOutput("pronostico_periodos"),
                            
                            tags$hr(),
                            
                            h4("Serie de tiempo con pronosticos puntuales"),
                            plotOutput("pronostico_puntuales")
                  
                   )#parÃ©ntesis tabPanel interior
                 )#parÃ©ntesis tabsetPanel Modelo 1
               )#parÃ©ntesis mainPanel Modelo 1
      ),#parÃ©ntesis tabPanel Modelo1
      
      tabPanel("MODELO LOG CUBICO ESTACIONAL",
               
               titlePanel("Analisis modelo cubico estacional"),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Modelo cubico estacional",
                            h4("Summary modelo cubico estacional"),
                            verbatimTextOutput("summary_mod_cubico"),
                            
                            tags$hr(),
                            h4("Grafico de residuales en escale LOG con limites 2 sigma"),
                            plotOutput("residuales_2sigma"),
                            
                            tags$hr(),
                            h4("Pronosticando periordos t=152 a 155 en escala original"),
                            tableOutput("pronostico_periodos_cubico"),
                            
                            tags$hr(),
                            h4("Graficando serie cos ajustes y pronosticos"),
                            plotOutput("pronostico_ajuste_cubico")
                     
                   )#parentesistabpanel
                 )#parentesis tabsetpanel modelo cubico estacional
               )#parentesis main panel modelo cubico estacional
      ),#parÃ©ntesis Modelo log cÃºbico estacional
      
      tabPanel("COMPARATIVO",
               titlePanel("Analisis grafico de modelos"),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Comparativo grafico",
                            h4("Comparativo entre los distintos modelos"),
                            plotOutput("comparativo_primario")
                     
                     
                   )#parentesis tabpanel interior comparativo grafico
                 )#parentesis tabsetpanel comparativo grafico
               )#parentesis main panel comparativo grafico
      )#parentesis tabpanel comparativo grafico
    )#ParÃ©ntesis NavbarPage
  )#parÃ©ntesis fuidPage

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
    plot.ts(data_st, ylim=c(min(data_st),max(data_st)), type = 'o', lwd = 2,col="darkorange")
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
      plot(logdata,ylim=c(min(logdata),max(logdata)),col="darkorange", lwd = 2)
      title(main="Log(data)")
      grid()
          })#anterior
    
    #GRAFICAR AUTOCORRELACION DATOS 
    output$pac_Data <- renderPlot({
      data_st <- data_l()
      acf(data_st,col="darkorange", lwd = 2)
    })#anterior
    
    #GRAFICAR AUTOCORRELACION PARCIAL DATOS  
    output$pacf_Data <- renderPlot({
      data_st <- data_l()
      pacf(data_st,col="darkorange", lwd = 2)
    })#anterior

    #GRAFICAR PAC LOGDATOS
    output$pacLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      acf(logdata,col="darkorange", lwd = 2)
      
    })#anterior
    
    #GRAFICAR PACF LOGDATOS
    output$pacfLogData <- renderPlot({
      data_st <- data_l()
      logdata <- log(data_st)
      pacf(logdata,col="darkorange", lwd = 2)
    })#anterior
    
    #GRAFICAR ESTACIONALIDAD
    output$estacionalidad <- renderPlot({
      data_st <- data_l()
      boxplot(data_st~cycle(data_st),names=month.abb,col="darkorange", lwd = 2)
      title(main="Estacionalidad")
      grid()
    })#anterior
    #
    #GRAFICAR DESCOMPOSICION MULTIPLICATIVA DE LA SERIE
    output$descomp_mult_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type = "multiplicative"),col="darkorange", lwd = 2)
    })#anterior
    
    #GRAFICAR DESCOMPOSICION ADITIVA DEL LOGARITMO
    output$descomp_adit_Log <- renderPlot({
      data_st <- data_l()
      plot(decompose(log(data_st), type="additive"),col="darkorange", lwd = 2)
    })#anterior
    
    #GRAFICAR DESCOMPOSICION ADITIVA DE LA SERIR
    output$descomp_adit_data <- renderPlot({
      data_st <- data_l()
      plot(decompose(data_st, type="additive"),col="darkorange", lwd = 2)
    })#anterior
    
    #GRAFICAR TENDENCIA POR DESCOMPOSION DE LA SERIE
    output$tendencia_desc_data <- renderPlot({
      data_st <- data_l()
      Tt=decompose(data_st,type="additive")$trend
      plot(Tt,ylim=c(min(data_st),max(data_st)),col="darkorange", lwd = 2,main="Componente tendencia estimada por descomposicion
     clasica\ grafico ajustado a rango de variacion de la serie")
      grid()
      })#anterior
    
    #GRAFICAR TENDENCIA POR DESCOMPOSION DEL LOGARITMO DE LA SERIE
    output$tendencia_desc_logdata <- renderPlot({
      data_st <- data_l()
      log_data <- (data_st)
      Tt_log=decompose(log_data,type="multiplicative")$trend
      plot(Tt_log,ylim=c(min(data_st),max(data_st)),col="darkorange", lwd = 2,main="Componente tendencia estimada por descomposicion
           clAsica\ grAfico ajustado a rango de variacion del log de la serie")
      grid()
    })#anterior
    
      #GRAFICAR ERROR DATOS Y LOGDATOS ADITIVA
      output$error_datos_logdatos <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        Tt_error_datos=decompose(data_st,type="additive")$random
        plot(Tt_error_datos,main="Componente error estimada por descomposicion clasica aditiva",col="darkorange", lwd = 2)
      grid()
    })#anterior
      
      #GRAFICAR ERROR DATOS Y LOGDATOS MULTIPLICATIVA
      output$error_datos_logdatos_multi <- renderPlot({
        data_st <- data_l()
        log_data <- log(data_st)
        error_datos_logdatos_multi=decompose(data_st,type="multiplicative")$random
        plot(error_datos_logdatos_multi,main="Componente error estimada por descomposicion clasica multiplicativa",col="darkorange", lwd = 2)
        grid()
      })#anterior
      
      #GRAFICAR PERIODODOGRAMA
      output$periodograma_log <- renderPlot({
        data_st <- data_l()
        log_data <- (data_st)
        periodogram(diff(log(data_st)),col="darkorange") #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        grid()
      })
      
      #GRAFICAR SERIE DE TIEMPO DE VALORES AJUSTADOS EN ESCALA ORIGINAL
      output$st_ajustado_original <- renderPrint({
        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #Â´indice de tiempo
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
        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #Â´indice de tiempo
        yt2=ts(data_st[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))
        #trimestre=relevel(trimestre,ref="4Q")
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(input$year,input$month))
        plot.ts(ythat1)
      })
      
      #GRAFICO DE RESIDUALES EN ESCALA LOG CON LIMITES 2sigma
      output$residuales2sigma <- renderPlot({
        data_st <- data_l()
        log_data <- log(data_st)
        m = 4 #NÂ´umero de perÂ´iodos a pronosticar dentro de la muestra
        n = length(data_st)-m #tama~no de la muestra para el ajuste
        t = 1:n #Â´indice de tiempo
        yt2=ts(data_st[t],frequency=4,start=c(input$year,input$month)) #serie con las primeras n observaciones
        trimestre=seasonal(decompose(yt2))
        #trimestre=relevel(trimestre,ref="4Q")
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(input$year,input$month))
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\nModelo Log cuadratico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
      })
      
      ##PRONOSTICANDO PERÂ´IODOS t=152 a 155 EN ESCALA ORIGINAL
      output$pronostico_periodos <- renderTable({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
     
        ###########complemento del análisis descritivo
        periodogram(diff(log(yt))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        
        ################################
        
        #############Definiendo estacionalidad
        
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        
        #Definiendo factor para estacionalidad pero s´olo para la regresi´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")
        
        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        
        ###########Modelo 1
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
        #Serie de tiempo de valores ajustados en escala original
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(1979,1))
        ythat1
        #Calculando AIC y BIC usando C*n(p)
        resmod1.orig=yt2-ythat1 #residuos en la escala original. Usados s´olo para c´alcular AIC y BIC
        #aic1=crit.inf.resid(resmod1.orig,n.par=6)
        #aic1
        #bic1=crit.inf.resid(resmod1.orig,n.par=6,AIC="FALSE")
        #bic1
        #GR´AFICO DE RESIDUALES EN ESCALA LOG CON L´IMITES 2sigma
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadr´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\nModelo Log cuadr´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        #PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        predicciones1
      })
      
      
      #GRAFICO DE SERIE DE TIEMPO CON PRONOSTICOS PUNTUALES
      output$pronostico_puntuales <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        
        ###########complemento del análisis descritivo
        periodogram(diff(log(yt))) #periodograma sobre los logaritmos diferenciados
        abline(v=c(0.25,0.5),col=2,lty=2)
        
        ################################
        
        #############Definiendo estacionalidad
        
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        
        #Definiendo factor para estacionalidad pero s´olo para la regresi´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")
        
        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        
        ###########Modelo 1
        lnyt2=log(yt2)
        lnyt2
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        summary(modelo1)
        #Serie de tiempo de valores ajustados en escala original
        ythat1=ts(exp(fitted(modelo1))*exp(summary(modelo1)$sigma^2/2),frequency=4,start=c(1979,1))
        ythat1
        #Calculando AIC y BIC usando C*n(p)
        resmod1.orig=yt2-ythat1 #residuos en la escala original. Usados s´olo para c´alcular AIC y BIC
        #aic1=crit.inf.resid(resmod1.orig,n.par=6)
        #aic1
        #bic1=crit.inf.resid(resmod1.orig,n.par=6,AIC="FALSE")
        #bic1
        #GR´AFICO DE RESIDUALES EN ESCALA LOG CON L´IMITES 2sigma
        plot.ts(residuals(modelo1),main="Residuos vs. t en escala Log\nModelo Log cuadr´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        plot(fitted(modelo1),residuals(modelo1),main="Residuos vs. ajustados en escala Log\nModelo Log cuadr´atico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1)$sigma,2*summary(modelo1)$sigma),col=2)
        #PRONOSTICANDO PER´IODOS t=152 a 155 EN ESCALA ORIGINAL
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        #serie de tiempo de los pron´osticos puntuales
        ytpron1=ts(predicciones1[,1],frequency=4,start=c(2016,4)) #los pron´osticos comienzan desde 1993-Q4
        accuracy(ytpron1,ytf) #Calculando exactitud de los pron´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRON´OSTICOS
        plot(yt,main="Serie real, ajustes y pron´osticos\nModelo Log cuadr´atico estacional")
        lines(ythat1,col=2)
        lines(ytpron1,col=4)
        legend("topleft",legend=c("Original","Ajustada","Pron´osticos"),col=c(1,2,4),lty=1)
      })
      
      #SUMMARY MODELO CUBICO ESTACIONAL
      output$summary_mod_cubico <- renderPrint({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        trimestre=season(yt2)
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        summary(modelo1b)
      })#anterior
      
      #GRAFICA RESIDUALES LIMITES 2 SIGMA
      output$residuales_2sigma <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        trimestre=season(yt2)
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        plot.ts(residuals(modelo1b),main="Residuos vs. t en escala Log\nModelo Log cubico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1b)$sigma,2*summary(modelo1b)$sigma),col=2)
        plot(fitted(modelo1b),residuals(modelo1b),main="Residuos vs. ajustados en escala Log\nModelo Log c´ubico estacional")
        abline(h=0,col=2)
        abline(h=c(-2*summary(modelo1b)$sigma,2*summary(modelo1b)$sigma),col=2)
      })#anterior
      
      #TABLA PRONOSTICANDO PERIOROS t=152 A 155 EN ESCALA ORIGINAL
      output$pronostico_periodos_cubico <- renderTable({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero s´olo para la regresi´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")
        
        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        predicciones1b
      })#anterior
      
      #GRAFICANDO LA SERIE, SUS AJUSTES Y PRON´OSTICOS
      output$pronostico_ajuste_cubico <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero s´olo para la regresi´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")
        
        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        ythat1b=ts(exp(fitted(modelo1b))*exp(summary(modelo1b)$sigma^2/2),frequency=4,start=c(1979,1))
        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        #Convirtiendo en serie de tiempo a los pron´osticos puntuales
        ytpron1b=ts(predicciones1b[,1],frequency=4,start=c(2016,4)) #los pron´osticos comienzan desde 1993-Q4
        accuracy(ytpron1b,ytf) #Calculando exactitud de los pron´osticos
        #GRAFICANDO LA SERIE, SUS AJUSTES Y PRONOSTICOS
        plot(yt,main="Serie real, ajustes y pronosticos\nModelo Log cubico estacional")
        lines(ythat1b,col=2)
        lines(ytpron1b,col=4)
        legend("topleft",legend=c("Original","Ajustada","Pronosticos"),col=c(1,2,4),lty=1)
      })#anterior
      
      output$comparativo_primario <- renderPlot({
        data_st <- data_l()
        yt=ts(data_st,frequency=4,start=c(1979,1)) #serie con todas las observaciones
        m=4 #N´umero de per´iodos a pronosticar dentro de la muestra
        n=length(yt)-m #tama~no de la muestra para el ajuste
        t=1:n #´indice de tiempo
        yt2=ts(yt[t],frequency=4,start=c(1979,1)) #serie con las primeras n observaciones
        #Definiendo factor para estacionalidad pero s´olo para la regresi´on lineal modelos 1 y 1b
        trimestre=season(yt2)
        trimestre=relevel(trimestre,ref="4Q")
        
        ######Para el pronóstico
        tnuevo=c((n+1):(n+m)) #definiendo valor de t para las m observaciones a pronosticar
        trimestre.nuevo=season(yt)[(length(yt2)+1):length(yt)] #definiendo trimestre correspondiente a tiempos de pron´ostico,
        #estos s´olo para la regresi´on lineal
        ytf=ts(yt[tnuevo],frequency=4,start=c(2016,4)) #Tomando de la serie completa los ´ultimos m valores que son pronosticados
        lnyt2=log(yt2)
        modelo1=lm(lnyt2~t+I(t^2)+trimestre)
        modelo1b=lm(lnyt2~t+I(t^2)+I(t^3)+trimestre)
        ythat1b=ts(exp(fitted(modelo1b))*exp(summary(modelo1b)$sigma^2/2),frequency=4,start=c(1979,1))
        
        predicciones1=exp(predict(modelo1,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                  interval="prediction"))*exp(summary(modelo1)$sigma^2/2)
        predicciones1=ts(predicciones1,frequency=4,start=c(2016,4))
        ytpron1=ts(predicciones1[,1],frequency=4,start=c(2016,4)) #los pron´osticos comienzan desde 1993-Q4
        
        predicciones1b=exp(predict(modelo1b,data.frame(t=tnuevo,trimestre=trimestre.nuevo),
                                   interval="prediction"))*exp(summary(modelo1b)$sigma^2/2)
        predicciones1b=ts(predicciones1b,frequency=4,start=c(2016,4))
        #Convirtiendo en serie de tiempo a los pron´osticos puntuales
        ytpron1b=ts(predicciones1b[,1],frequency=4,start=c(2016,4)) #los pron´osticos comienzan desde 1993-Q4
        
        
        plot(ytf,ylim=c(130000,200000),lty=1,col="darkorange",type="b",pch=19,
             main="Valores reales y pronosticados\nProduccion trimestral cemento portland 1993Q4-1994Q3",xaxt="n")
        axis(1,at=time(ytf),labels=c("2016Q4","2017Q1","2017Q2","2017Q3"))
        lines(ytpron1,lty=2,col="red",type="b",pch=2)
        lines(ytpron1b,lty=3,col="blue",type="b",pch=3)
        legend("topleft",legend=c("Real","Log-cuadratico estacional","Log-cubico estacional"),
               lty=c(1:7),pch=c(19,2:7),col=c("darkorange","red","blue","orange","brown","darkgreen","darkred"))
        
      })
      
}#parÃ©ntesis para cerrar el server
    

# Create Shiny app ----
shinyApp(ui, server)
