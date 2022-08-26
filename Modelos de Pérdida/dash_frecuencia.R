# Dashboard de Clase (a,b,0) y (a,b,1)
library(shiny)
library(shinydashboard) #<- para crear dashboards con shiny
library(actuar) #<- funcion coveraage 
library(MASS) # <- distribuciones
library(ggplot2) # <- graficas 
library(shinyWidgets) # <- widgets para el dashboard 
library(tidyr)

 ##############

distributions <- c('Binomial',
                   'Poisson',
                   'Binomial Negativa',
                   'Geométrica')

header <- dashboardHeader(title = "Modelos de Frecuencia")

sidebar <- dashboardSidebar(
  radioGroupButtons(
    inputId = "dist1",
    label = "Selecciona una Distribucion", 
    choices = distributions,
    status = "primary"
  ),
  numericInput('ene','Selecciona el Valor de n',
               value = 10, min = 0,max = 30),
  numericInput('pe','Selecciona el Valor de p',
               value = .3, min = 0,max = 1,step = .05),
  numericInput('pe0','Selecciona el Valor de p0',
               value = .3, min = 0,max = 1,step = .05),
  numericInput('lambda','Selecciona el Valor de lambda',
               value = 2, min = 0,max = 10),
  numericInput('ere','Selecciona el Valor de r',
               value = 10, min = 0,max = 10),
  br())
body <- dashboardBody(
  
  fluidRow(
    tabBox( width = 12,
      title = "Comparativo Variable Original vs Modificada y Truncada",
      tabPanel("Funcion de Densidad", plotOutput("grafica1") ),
      tabPanel("Funcion de Distribucion", plotOutput("grafica2") ),
      tabPanel("Funcion de Sobrevivencia", plotOutput('grafica3'))
    )
  )
)

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header,sidebar,body,skin = 'blue')
server <- function(input, output) {
  
  #barras de densidad
  cur_den <-  function(v, type) {
    switch(type,
           'Poisson'= dpois(v,input$lambda),
           'Binomial'= dbinom(v,input$ene,input$pe),
           'Binomial Negativa'= dnbinom(v,input$ere,input$pe),
           'Geométrica'= dgeom(v,input$pe)
    )
  }
  cur_dis <-  function(v, type) {
    switch(type,
           'Poisson'= ppois(v,input$lambda),
           'Binomial'= pbinom(v,input$ene,input$pe),
           'Binomial Negativa'= pnbinom(v,input$ere,input$pe),
           'Geométrica'= pgeom(v,input$pe)
    )
  }
  
  #barras de densidad modificada 
  cur_denm <-  function(v, type) {
    switch(type,
           'Poisson'= dzmpois(v,input$lambda,input$pe0),
           'Binomial'= dzmbinom(v,input$ene,input$pe,input$pe0),
           'Binomial Negativa'= dzmnbinom(v,input$ere,input$pe,input$pe0),
           'Geométrica'= dzmgeom(v,input$pe,input$pe0)
    )
  }
  cur_dism <-  function(v, type) {
    switch(type,
           'Poisson'= pzmpois(v,input$lambda,input$pe0),
           'Binomial'= pzmbinom(v,input$ene,input$pe,input$pe0),
           'Binomial Negativa'= pzmnbinom(v,input$ere,input$pe,input$pe0),
           'Geométrica'= pzmgeom(v,input$pe,input$pe0)
    )
  }
  #barras de densidad truncada 
  cur_dent <-  function(v, type) {
    switch(type,
           'Poisson'= dztpois(v,input$lambda),
           'Binomial'= dztbinom(v,input$ene,input$pe),
           'Binomial Negativa'= dztnbinom(v,input$ere,input$pe),
           'Geométrica'= dztgeom(v,input$pe)
    )
  }
  cur_dist <-  function(v, type) {
    switch(type,
           'Poisson'= pztpois(v,input$lambda),
           'Binomial'= pztbinom(v,input$ene,input$pe),
           'Binomial Negativa'= pztnbinom(v,input$ere,input$pe),
           'Geométrica'= pztgeom(v,input$pe)
    )
  }
  #Gráfica1 Densidad 
  output$grafica1 <- renderPlot({
    typ1 <- input$dist1
    n <- input$ene
    
    fun.1 <- function(x) cur_den(x,typ1)
    fun.2 <- function(x) cur_denm(x,typ1)
    fun.3 <- function(x) cur_dent(x,typ1)
    
    x <- c(0:n)
    
    Original <- fun.1(x)
    Modificada <- fun.2(x)
    Truncada <- fun.3(x)
    
    df1 <- data.frame(x,Original,Modificada,Truncada)
    df2 <- tidyr::pivot_longer(df1, cols=c('Original', 'Modificada','Truncada'), names_to='funcion', 
                               values_to="value")
    
    ggplot(data = df2, mapping = aes(x = x, y = value, fill = funcion)) + 
        geom_bar(stat= 'identity') +
        xlab("x") + ylab("Perdida") + 
        facet_wrap(~ funcion)
  })
  
  #Gráfica2 Distribucion  
  output$grafica2 <- renderPlot({
    typ1 <- input$dist1
    n <- input$ene
    
    fun.1 <- function(x) cur_dis(x,typ1)
    fun.2 <- function(x) cur_dism(x,typ1)
    fun.3 <- function(x) cur_dist(x,typ1)
    
    x <- c(0:n)
    
    Original <- fun.1(x)
    Modificada <- fun.2(x)
    Truncada <- fun.3(x)
    
    df1 <- data.frame(x,Original,Modificada,Truncada)
    df2 <- tidyr::pivot_longer(df1, cols=c('Original', 'Modificada','Truncada'), names_to='funcion', 
                               values_to="value")
    
    ggplot(data = df2, mapping = aes(x = x, y = value, fill = funcion)) + 
      geom_bar(stat= 'identity') +
      xlab("x") + ylab("Perdida") + 
      facet_wrap(~ funcion)  
  })
  #Grafica 3 Sobrevivencia
  output$grafica3 <- renderPlot({
    typ1 <- input$dist1
    n <- input$ene
    
    fun.1 <- function(x) cur_dis(x,typ1)
    fun.2 <- function(x) cur_dism(x,typ1)
    fun.3 <- function(x) cur_dist(x,typ1)
    
    x <- c(0:n)
    
    Original <- 1 - fun.1(x)
    Modificada <- 1 - fun.2(x)
    Truncada <- 1 - fun.3(x)
    
    df1 <- data.frame(x,Original,Modificada,Truncada)
    df2 <- tidyr::pivot_longer(df1, cols=c('Original', 'Modificada','Truncada'), names_to='funcion', 
                               values_to="value")
    
    ggplot(data = df2, mapping = aes(x = x, y = value, fill = funcion)) + 
      geom_bar(stat = 'identity') +
      xlab("x") + ylab("Perdida") + 
      facet_wrap(~ funcion)  
  })
  
}
shinyApp(ui, server)