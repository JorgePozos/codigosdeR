##############Dashboard de V.A:###########################
#########################################################
library(shiny)
library(stats)
library(actuar)
library(shinythemes)
library(fitdistrplus) #Ajuste de curvas de Probabilidad

help("Distributions")#######distribuciones a usar 

distributions <- c("Beta",
                   "Cauchy",
                   "Gamma",
                   "Lognormal",
                   "Normal",
                   "Uniforme",
                   "Weibull",
                   "Exponencial",##########
                   "Distribución-F",
                   "T Student",
                   "Xi-Cuadrada")


ui <- fluidPage(
  titlePanel("Dashboard de Variables Aleatorias"),
  theme = shinythemes::shinytheme('superhero'),
  ######################## sidebarLayout (interfaz de usuario)######################## 
sidebarLayout(
    sidebarPanel(
      selectInput('dist1',label="Selecciona una Distribución",
                  choices = distributions),
      sliderInput(inputId = "n1",label="Número de Simulaciones",
                  value=500,min = 1,max = 1000),
      sliderInput(inputId = "pp1",label="Parametro 1",
                  value=50,min=1,max=100),
      sliderInput(inputId = "pp2",label="Parametro 2",
                  value=50,min=1,max=100)),
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
#########################     ###################



server <- function(input, output) {
cur_den <-  function(v, type) {####curva densidad
    switch(type,
           "Normal" = dnorm(v,input$pp1,input$pp2),
           "Uniforme" = dunif(v,input$pp1,input$pp2),
           "Lognormal" = dlnorm(v,input$pp1,input$pp2),
           "Beta" = dbeta(v,input$pp1,input$pp2),
           "Binomial" =dbinom(v,input$pp1,input$pp2),
           "Cauchy"= dcauchy(v,input$pp1,input$pp2),
           "Xi-Cuadrada"= dchisq(v,input$pp1),
           "Exponencial" = dexp(v,input$pp1),
           "Distribución-F"=df(v,input$pp1,input$pp2),
           "Gamma"=dgamma(v,input$pp1,input$pp2),
           "Geométrica"=dgeom(v,input$pp1,input$pp2),
           "Hipergeómetrica"=dhyper(v,input$pp1,input$pp2),
           "Lognormal"=dlnorm(v,input$pp1,input$pp2),
           "Multinomial"=dmultinom(v,input$pp1,input$pp2),
           "Binomial Negativa"=dnbinom(v,input$pp1,input$pp2),
           "Normal"=dnorm(v,input$pp1,input$pp2),
           "Poisson"=dpois(v,input$pp1,input$pp2),
           "T Student"=dt(v,input$pp1),
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
         "Xi-Cuadrada"= pchisq(v,input$pp1),
         "Exponencial" = pexp(v,input$pp1),
         "Distribución-F"=df(v,input$pp1,input$pp2),
         "Gamma"=pgamma(v,input$pp1,input$pp2),
         "Geométrica"=pgeom(v,input$pp1,input$pp2),
         "Hipergeómetrica"=phyper(v,input$pp1,input$pp2),
         "Lognormal"=plnorm(v,input$pp1,input$pp2),
         "Multinomial"=pmultinom(v,input$pp1,input$pp2),
         "Binomial Negativa"=pnbinom(v,input$pp1,input$pp2),
         "Normal"=pnorm(v,input$pp1,input$pp2),
         "Poisson"=ppois(v,input$pp1,input$pp2),
         "T Student"=pt(v,input$pp1),
         "Uniforme"=punif(v,input$pp1,input$pp2),
         "Weibull"=pweibull(v,input$pp1,input$pp2))
}

paraqq <- function(v,type){####nombres de distribuciones
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
  data <- reactive({  ######Simulación de Datos
    dist1 <- switch(input$dist1,
                    "Normal" = rnorm(input$n1,input$pp1, input$pp2),
                    "Uniforme" = runif(input$n1,input$pp1, input$pp2),
                    "Lognormal" = rlnorm(input$n1,input$pp1, input$pp2),
                    "Exponencial" = rexp(input$n1,input$pp1),
                    "Beta" = rbeta(input$n1,input$pp1, input$pp2),
                    "Cauchy"= rcauchy(input$n1,input$pp1, input$pp2),
                    "Xi-Cuadrada"= rchisq(input$n1,input$pp1),
                    "Distribución-F"=rf(input$n1,input$pp1, input$pp2),
                    "Gamma"=rgamma(input$n1,input$pp1, input$pp2),
                    "Lognormal"=rlnorm(input$n1,input$pp1, input$pp2),
                    "Normal"=rnorm(input$n1,input$pp1, input$pp2),
                    "T Student"=rt(input$n1,input$pp1),
                    "Weibull"=rweibull(input$n1,input$pp1, input$pp2))
    
  })
  
  output$g1 <- renderPlot({######plot con datos
    plot(data(),main = paste("Datos Simulados"),col="darkorange2")
  })
  
  output$g2 <- renderPlot({#####histograma
    hist(data(), freq = FALSE, main = "Histograma",border = "white")
    typ1 <- input$dist1
    curve(cur_den(x,typ1), 
          col= "darkorange2", lwd = 5,
          add = TRUE)
    })
  output$g3 <- renderPlot({###########teorica vs empirica
    typ1 <- input$dist1
    plot(ecdf(data()))
    curve(cur_dis(x,typ1),col="darkorange2",
          main="Función de Distribución",add = TRUE)
    legend("topleft",lty = c(2,1),lwd = c(1,2),
           col=c("black","darkorange2"),legend = c("Empírica","Teórica"))
  })
  output$t1 <- renderPrint({#######resumen est. decriptiva
    summary(data())})
  
  output$g4 <- renderPlot({#######3Caja y brazos
    boxplot(data())
  })
  
  output$g5 <- renderPlot({######### densidad teórica
    typ1 <- input$dist1
    curve(cur_den(x,typ1), xlim= c(-5, 5), col= "darkorange2", lwd = 5)
  })
  
  output$g6 <- renderPlot({######### Distribución Teórica
    typ1 <- input$dist1
    curve(cur_dis(x,typ1), xlim= c(0, 1), col= "darkorange2", lwd = 5)
  })
  
  output$g7 <- renderPlot({#########3 Sobrevivencia
    typ1 <- input$dist1
    curve(1 - cur_dis(x,typ1), xlim= c(0, 1), col= "darkorange2", lwd = 5)
  })
  
  output$g8 <- renderPlot({########## qqplot 
    mod1 <- fitdist(data(),paraqq(2,input$dist1),method = c("mle"))
    qqcomp(mod1) #Probabilidades Teóricas vs Empiricas 
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

