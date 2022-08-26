#install.packages()
library(shiny)
library(shinydashboard)
library(ggplot2) # graficas 
library(shinyWidgets) # widgets para el dashboard 
library(tidyr)


#Datos de Cartera
  cartera <- c("Berkshire Hathaway B"="BRK-B",
               "Amazon"="AMZN",
               "Tesla"="TSLA",
               "Unitedhealth Group Inc"="UNH",
               "Johnson & Johnson"="JNJ",
               "Apple Inc."="AAPL")
  
stockEnv <- new.env()
opciones <- c(  "Ninguna",
                "Promedio Movil 7 dias",
                "Promedio Movil 50 dias", 
                "Promedio Movil Exponencial 7 dias",
                "Promedio Movil Exponencial 50 dias",
                "Banda superior e inferior",
                "Momentum",
                "ROC",
                "RSI")

symbols <- getSymbols(cartera, src='yahoo', env=stockEnv,
                      from="2021-01-01",to="2022-01-01")

# Se Definen Partes de la APP
  header <- dashboardHeader(title = "Analisis Técnico con R")

  sidebar <- dashboardSidebar(width = 350,
    radioGroupButtons( 'accion',
    label = "Selecciona una Accion", 
    choices = cartera), br(),
    radioGroupButtons('opciones',
      label = 'Indice',
      choices = opciones
    ))

  body <- dashboardBody(
    box( title= 'Analisis Tecnico',
    tabsetPanel(type = "tabs",
              tabPanel("Linea", plotOutput("g1")),
              tabPanel("Barras", plotOutput("g2")),
              tabPanel("Velas", plotOutput("g3"))
              )
    ,width = 12
    )
  )

# Se integran partes de APP
  ui<-dashboardPage(
    header,
    sidebar,
    body
  )

# Server
  server <- function(input, output) { 
    indice <- reactive({function(type){
      switch (type,
            "Promedio Movil 7 dias" = addSMA(n=7,on=1,col='turquoise4'),
            "Promedio Movil 50 dias" = addSMA(n=50,on=1,col='violetred2'), 
            "Promedio Movil Exponencial 7 dias"= addEMA(n=7,on=1,col='coral'),
            "Promedio Movil Exponencial 50 dias"=addEMA(n=7,on=1,col='navy'),
            "Banda superior e inferior"=addBBands(n=15, sd=2),
            "Momentum"=addMomentum(n=1),
            "ROC"=addROC(n=1),
            "RSI"=addRSI(n=14, maType = 'EMA')
             )
            }
       })
    
    output$g1 <- renderPlot({
      typ1 <- input$accion
      index <- input$opciones
      
      
      if (index =="Ninguna") {
        chartSeries(stockEnv[[typ1]],   name=typ1,type="line") 
      } else {
        chartSeries(stockEnv[[typ1]],   name=typ1,type="line")
         indice()(index)
      }
    })
    
    ### Barras
    output$g2<-renderPlot({
      typ1 <- input$accion
      index <- input$opciones
      
      
      if (index =="Ninguna") {
        chartSeries(stockEnv[[typ1]],   name=typ1,type="bar") 
      } else {
        chartSeries(stockEnv[[typ1]],   name=typ1,type="bar")
         indice()(index)
      }
    })
    ### Velas
    output$g3<-renderPlot({
      typ1 <- input$accion
      index <- input$opciones
      
      
      if (index =="Ninguna") {
        chartSeries(stockEnv[[typ1]], name=typ1,type="candlesticks") 
      } else {
        chartSeries(stockEnv[[typ1]], name=typ1,type="candlesticks")
         indice()(index)
      }
      
    })
    
}

shinyApp(ui, server)
