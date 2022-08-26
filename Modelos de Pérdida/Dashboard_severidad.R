########## Dashboard de Severidad ##############
library(shiny)
library(stats)
library(actuar)
library(shinythemes)
library(fitdistrplus) #Ajuste de curvas de Probabilidad
library(MASS) #distr de probabilidad 
library(actuar) #modificaciones en la v-a- de pérdida X

help("Distributions")#######distribuciones a usar 

distributions <- c("Gamma",
                   "Uniforme",
                   "Exponencial",
                   "Lognormal",
                   "Weibull",
                   "Xi-Cuadrada",
                   "Pareto",
                   "Gumbel",
                   "Exponencial inversa",
                   "Inversa paralogistica",
                   "Pareto inversa",
                   "Transformada gamma inversa",
                   "Gamma inversa",
                   "Weibull inversa",
                   "Log gamma",
                   "Log logistica",
                   "Paralogistica")


ui <- fluidPage(
  titlePanel("Dashboard de Modelo Severidad"),
  sidebarLayout(
    sidebarPanel(
      selectInput('dist1',label="Selecciona una Distribución",
                  choices = distributions),
      sliderInput(inputId = "n1",label="Escala eje_x",
                  value=5,min = 1,max = 50),
      sliderInput(inputId = "n2",label="Escala eje_y",
                  value=1,min = 1,max = 50),
      sliderInput(inputId = "pp1",label="Parametro 1",
                  value=4,min=1,max=100),
      sliderInput(inputId = "pp2",label="Parametro 2",
                  value=3,min=1,max=100),
      sliderInput('dedu','Selecciona Deducible',
                  value = .5, min = 0,max = 1),
      sliderInput('limi','Selecciona Limite de Poliza',
                  value = 1, min = 0,max = 10),
      sliderInput('coa','Selecciona Coaseguro',
                  value = .9, min = 0,max = 1),
      sliderInput('infla','Selecciona Inflación',
                  value = 0.05, min = 0,max = 1),
      br(),
      checkboxInput('franq', 'Deducible de Franquicia', value = FALSE),
      checkboxInput('perdi', 'Variable de Perdida', value = FALSE)),
    
    mainPanel(
      tabsetPanel(
        tabPanel('Densidad',
                 plotOutput("grafica")))
    )
  )
)
 
server <- function(input, output) {
  #Curvas de densidad
  cur_den <-  function(v, type) {
    switch(type,
           "Gamma"=dgamma(v,input$pp1,input$pp2),
           "Uniforme" = dunif(v,input$pp1,input$pp2),
           "Exponencial" = dexp(v,input$pp1),
           "Lognormal" = dlnorm(v,input$pp1,input$pp2),
           "Weibull"=dweibull(v,input$pp1,input$pp2),
           "Xi-Cuadrada"= dchisq(v,input$pp1),
           "Pareto"=dpareto(v,input$pp1,input$pp2),
           "Gumbel"=dgumbel(v,input$pp1,input$pp2),
           "Exponencial inversa"=dinvexp(v,input$pp1),
           "Paralogistica" = dparalogis(v,input$pp1,input$pp2),
           "Inversa paralogistica"= dinvparalogis(v,input$pp1,input$pp2),
           "Pareto inversa"=dinvpareto(v,input$pp1,input$pp2),
           "Transformada gamma inversa"=dinvtrgamma(v,input$pp1,input$pp2),
           "Gamma inversa"= dinvgamma(v,input$pp1,input$pp2),
           "Weibull inversa"= dinvweibull(v,input$pp1,input$pp2),
           "Log gamma"= dlgamma(v,input$pp1,input$pp2),
           "Log logistica"= dllogis(v,input$pp1,input$pp2),
           )
  }
  
  coverage_m1 <- function(type) {
    ####curva de la var. modificada 
    switch(type,
           "Gamma"= coverage(dgamma,pgamma,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi), 
           "Uniforme" = coverage(dunif,punif,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Exponencial" = coverage(dexp,pexp,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Lognormal" = coverage(dlnorm,plnorm,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Weibull"= coverage(dweibull,pweibull,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Xi-Cuadrada"= coverage(dchisq,pchisq,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Pareto"= coverage(dpareto,ppareto,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Gumbel"= coverage(dgumbel,pgumbel,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Exponencial inversa"= coverage(dinvexp,pinvexp,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Paralogistica" = coverage(dparalogis,pparalogis,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Inversa paralogistica"=coverage(dinvparalogis,pinvparalogis,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Pareto inversa"=coverage(dinvpareto,pinvpareto,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Transformada gamma inversa"= coverage(dinvtrgamma,pinvtrgamma,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Gamma inversa" = coverage(dinvgamma,pinvgamma,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Weibull inversa"= coverage(dinvweibull,pinvweibull,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Log gamma"= coverage(dlgamma,plgamma,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           "Log logistica"= coverage(dllogis,pllogis,input$dedu, input$franq,  input$limi,  input$coa,  input$infla,input$perdi),
           
           )
  }

  output$grafica <- renderPlot({######### densidad teórica
    typ1 <- input$dist1
    a <- input$pp1
    b <- input$pp2
    eje <- input$n1
    ejey <- input$n2
    
    if(typ1 == "Exponencial" | typ1 == "Xi-Cuadrada"| typ1 == "Exponencial inversa"){
      
      fun.1 <- function(x) cur_den(x,typ1)
      fun.2 <- function(x) coverage_m1(typ1)(x,a)
      
      (ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
        stat_function(fun = fun.1, color='blue') +
        stat_function(fun=fun.2)+ xlim(0,eje)+ylim(0,ejey))
      
      #curve(cur_den(x,typ1), xlim = c(0,eje),ylim=c(0,ejey),add = 1) #densidad 
      #curve(coverage_m1(typ1)(x,a), xlim = c(0,eje), ylim = c(0,ejey), #severidad
      #      col = 'green', add = TRUE)
    }else{
      fun.1 <- function(x) cur_den(x,typ1)
      fun.2 <- function(x) coverage_m1(typ1)(x,a,b)
      
      (ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
          stat_function(fun = fun.1, color='blue') +
          stat_function(fun=fun.2)+ xlim(0,eje)+ylim(0,ejey))
      }
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

