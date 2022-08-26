##############Dashboard de V.A:###########################
#########################################################
library(shiny)
library(stats)
library(actuar)
library(shinythemes)
library(fitdistrplus) #Ajuste de curvas de Probabilidad

help("Distributions")

distributions <- c("Beta",
                   "Cauchy",
                   "Distribución-F",
                   "Gamma",
                   "Lognormal",
                   "Normal",
                   "Uniforme",
                   "Weibull")
distributions_1 <- c("Exponencial",##########
                     "T Student",
                     "Xi-Cuadrada"
                     )
ui <- fluidPage(
  titlePanel("Dashboard de Variables Aleatorias"),
  theme = shinythemes::shinytheme('superhero'),
#######################  Layout 1parámetro ####################
  sidebarLayout(
    sidebarPanel(
      selectInput('dist',label="Distribuciones de un Solo Parámetro",
                  choices = distributions_1),
      br(),
      sliderInput("n",
                  "Número de Simulaciones",
                  value = 500,
                  min = 1,
                  max = 1000),
      sliderInput(inputId = "p1",label="Parametro 1",
                  value=.5,min=0,max=1)
      
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel('Datos',
                           plotOutput("gg1")),
                  tabPanel('Histograma',
                           plotOutput("plot")),
                  tabPanel('Empirica vs Teórica',
                           plotOutput("gg3")),
                  tabPanel('Caja',
                           plotOutput("gg4")),
                  tabPanel('Densidad',
                           plotOutput("gg5")),
                  tabPanel('Distribución',
                           plotOutput("gg6")),
                  tabPanel('Sobrevivencia',
                           plotOutput("gg7")),
                  tabPanel('QQ-Plot',
                            plotOutput("gg8"))),
      verbatimTextOutput("tt1")
    )
  ),
#########################  Panel 2 Distr. #######################
  sidebarLayout(
    sidebarPanel(
      selectInput('dist1',label="Distribuciones de Dos Parémetros",
                  choices = distributions),
      sliderInput(inputId = "n1",label="Número de Simulaciones",
                  value=500,min = 1,max = 1000),
      sliderInput(inputId = "pp1",label="Parametro 1",
                  value=1,min=0,max=100),
      sliderInput(inputId = "pp2",label="Parametro 2",
                  value=.5,min=0,max=1)),
    mainPanel(
      tabsetPanel(
        tabPanel('Datos',
                 plotOutput("g1")),
        tabPanel('Histograma',
                 plotOutput("g2")),
        tabPanel('Empirica vs Teórica',
                 plotOutput("g3")),
        tabPanel('Caja',
                 plotOutput("g4")),
        tabPanel('Densidad',
                 plotOutput("g5")),
        tabPanel('Distribución',
                 plotOutput("g6")),
        tabPanel('Sobrevivencia',
                 plotOutput("g7")),
        tabPanel('QQ-Plot',
               plotOutput("g8"))),
      verbatimTextOutput("t1")
    )
  )
)

#########################  Panel Principal  ###################
#########################  Panel Principal  ###################
#########################  Panel Principal  ###################

server <- function(input, output) {
################server 1 #####################dist, n, p1
cur_den <-  function(v, type) {####curva densidad
    switch(type,
           "Normal" = dnorm(v,input$pp1,input$pp2),
           "Uniforme" = dunif(v,input$pp1,input$pp2),
           "Lognormal" = dlnorm(v,input$pp1,input$pp2),
           "Beta" = dbeta(v,input$pp1,input$pp2),
           "Binomial" =dbinom(v,input$pp1,input$pp2),
           "Cauchy"= dcauchy(v,input$pp1,input$pp2),
           "Xi-Cuadrada"= dchisq(v,input$p1),
           "Exponencial" = dexp(v,input$p1),
           "Distribución-F"=df(v,input$pp1,input$pp2),
           "Gamma"=dgamma(v,input$pp1,input$pp2),
           "Geométrica"=dgeom(v,input$pp1,input$pp2),
           "Hipergeómetrica"=dhyper(v,input$pp1,input$pp2),
           "Lognormal"=dlnorm(v,input$pp1,input$pp2),
           "Multinomial"=dmultinom(v,input$pp1,input$pp2),
           "Binomial Negativa"=dnbinom(v,input$pp1,input$pp2),
           "Normal"=dnorm(v,input$pp1,input$pp2),
           "Poisson"=dpois(v,input$pp1,input$pp2),
           "T Student"=dt(v,input$p1),
           "Uniforme"=dunif(v,input$pp1,input$pp2),
           "Weibull"=dweibull(v,input$pp1,input$pp2))
  }
  
cur_dis <-  function(v, type) {####curva distribucion
  switch(type,
         "Normal" = pnorm(v,input$pp1,input$pp2),
         "Uniforme" = punif(v,input$pp1,input$pp2),
         "Lognormal" = plnorm(v,input$pp1,input$pp2),
         "Beta" = pbeta(v,input$pp1,input$pp2),
         "Binomial" =pbinom(v,input$pp1,input$pp2),
         "Cauchy"= pcauchy(v,input$pp1,input$pp2),
         "Xi-Cuadrada"= pchisq(v,input$p1),
         "Exponencial" = pexp(v,input$p1),
         "Distribución-F"=df(v,input$pp1,input$pp2),
         "Gamma"=pgamma(v,input$pp1,input$pp2),
         "Geométrica"=pgeom(v,input$pp1,input$pp2),
         "Hipergeómetrica"=phyper(v,input$pp1,input$pp2),
         "Lognormal"=plnorm(v,input$pp1,input$pp2),
         "Multinomial"=pmultinom(v,input$pp1,input$pp2),
         "Binomial Negativa"=pnbinom(v,input$pp1,input$pp2),
         "Normal"=pnorm(v,input$pp1,input$pp2),
         "Poisson"=ppois(v,input$pp1,input$pp2),
         "T Student"=pt(v,input$p1),
         "Uniforme"=punif(v,input$pp1,input$pp2),
         "Weibull"=pweibull(v,input$pp1,input$pp2))
}
  d <- reactive({
    dist <- switch(input$dist,
                   "Beta" = rbeta(input$n, input$p1),
                   "Cauchy"= rcauchy(input$n, input$p1),
                   "Xi-Cuadrada"= rchisq(input$n, input$p1),
                   "Exponencial" = rexp(input$n, input$p1),
                   "Distribución-F"=rf(input$n, input$p1),
                   "Gamma"=rgamma(input$n, input$p1),
                   "Lognormal"=rlnorm(input$n, input$p1),
                   "Normal"=rnorm(input$n, input$p1),
                   "T Student"=rt(input$n, input$p1),
                   "Uniforme"=runif(input$n, input$p1),
                   "Weibull"=rweibull(input$n, input$p1))
  })
  
  output$plot <- renderPlot({
    hist(d(),
         main = paste("Histograma"),
         freq = FALSE,
         col = "gray", border = "white")
    typ <- input$dist
    curve(cur_den(x,typ), xlim= c(0, 15), 
          col= "darkorange2", lwd = 5,
          add = TRUE)
  })
  
  output$gg1 <- renderPlot({
    plot(d(),
         main = paste("Datos Simulados"),col="darkorange2") 
  })
  
  output$gg3 <- renderPlot({
    typ <- input$dist
    plot(ecdf(d()),main = paste("Distribución"))
    curve(cur_dis(x,typ),col="red",
          main="Función de Distribución",add = TRUE)
    legend("topleft",lty = c(2,1),lwd = c(1,2),
           col=c("black","darkorange2"),legend = c("Empírica","Teórica"))
  })
  
  output$gg4 <- renderPlot({
    boxplot(d(), main = paste("Caja y Brazos"))
    
  })
  
  output$tt1 <- renderPrint({
    summary(d())})
  
  output$gg5 <- renderPlot({
    typ <- input$dist
    curve(cur_den(x,typ), xlim= c(input$p1 - 3, input$p1 + 3),
          col= "darkorange2",main = paste("Densidad"), lwd = 5)
  })
  
  output$gg6 <- renderPlot({
    typ <- input$dist
    curve(cur_dis(x,typ), xlim= c(0, 15),
          main = paste("Distribución"),col= "darkorange2", lwd = 5)
  })
  
  output$gg7 <- renderPlot({
    typ <- input$dist
    curve(1 - cur_dis(x,typ), xlim= c(0, 15), 
          main = paste("Sobrevivencia"),col= "darkorange2", lwd = 5)
  })
  paraqq <- function(v,type){
    switch(type,
           "Beta" = 'beta',
           "Binomial" ='binom',
           "Cauchy"= 'cauchy',
           "Xi-Cuadrada"= 'chisq',
           "Exponencial" = 'exp',
           "Distribución-F"='f',
           "Gamma"= 'gamma',
           "Lognormal"= 'lnorm',
           "Normal"= 'norm',
           "T Student"= 't',
           "Uniforme"= 'unif',
           "Weibull"= 'weibull')
  }
  
  output$gg8 <- renderPlot({
    
    mod1 <- fitdist(d(),paraqq(2,input$dist),method = c("mme"))
    qqcomp(mod1) #Probabilidades Teóricas vs Empiricas 
    })
  

  ############################server 2 #######dist1, n1, pp1,pp2
  
  
  data <- reactive({  
    dist1 <- switch(input$dist1,
                    "Normal" = rnorm,
                    "Uniforme" = runif,
                    "Lognormal" = rlnorm,
                    "Exponencial" = rexp,
                    "Beta" = rbeta,
                    "Binomial" =rbinom,
                    "Cauchy"= rcauchy,
                    "Xi-Cuadrada"= rchisq,
                    "Exponencial" = rexp,
                    "Distribución-F"=rf,
                    "Gamma"=rgamma,
                    "Geométrica"=rgeom,
                    "Hipergeómetrica"=rhyper,
                    "Lognormal"=rlnorm,
                    "Multinomial"=rmultinom,
                    "Binomial Negativa"=rnbinom,
                    "Normal"=rnorm,
                    "Poisson"=rpois,
                    "T Student"=rt,
                    "Uniforme"=runif,
                    "Weibull"=rweibull)
    dist1(input$n1,input$pp1, input$pp2)
    
  })
  
  output$g1 <- renderPlot({
    plot(data(),main = paste("Datos Simulados"),col="darkorange2")
  })
  
  output$g2 <- renderPlot({
    hist(data(), freq = FALSE, main = "Histograma",border = "white")
    typ1 <- input$dist1
    curve(cur_den(x,typ1), xlim= c(-15, 15), 
          col= "darkorange2", lwd = 5,
          add = TRUE)
    })
  
  output$g3 <- renderPlot({
    typ1 <- input$dist1
    plot(ecdf(data()))
    curve(cur_dis(x,typ1),col="darkorange2",
          main="Función de Distribución",add = TRUE)
    legend("topleft",lty = c(2,1),lwd = c(1,2),
           col=c("black","darkorange2"),legend = c("Empírica","Teórica"))
    
  })
  
  output$t1 <- renderPrint({
    summary(data())})
  
  output$g4 <- renderPlot({
    boxplot(data())
  })
  
  output$g5 <- renderPlot({
    typ1 <- input$dist1
    curve(cur_den(x,typ1), xlim= c(-5, 5), col= "darkorange2", lwd = 5)
  })
  
  output$g6 <- renderPlot({
    typ1 <- input$dist1
    curve(cur_dis(x,typ1), xlim= c(0, 1), col= "darkorange2", lwd = 5)
  })
  
  output$g7 <- renderPlot({
    typ1 <- input$dist1
    curve(1 - cur_dis(x,typ1), xlim= c(0, 1), col= "darkorange2", lwd = 5)
  })
  
  output$g8 <- renderPlot({
    mod1 <- fitdist(data(),paraqq(2,input$dist1),method = c("mme"))
    qqcomp(mod1) #Probabilidades Teóricas vs Empiricas 
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

