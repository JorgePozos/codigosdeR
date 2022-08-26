
###### Aplicación 1._ Variables Aleatorias #########

#librerias de variables aleatorias 
library(shiny)
library(actuaryr)
library(stats)
library(shinythemes)

help("distributions")

#Análisis de Variables Aleatorias

#Normal
n = 1000
mu= 2
s= 4

curve(dbeta(x, mu,s), xlim= c(0, 1), col= "blue")
curve(dnorm(x, mu,s), xlim= c(mu - 4*s, mu + 4*s), col= "blue", lwd = 5)
curve(pnorm(x, mu,s), xlim= c(mu - 4*s, mu + 4*s), col= "blue", lwd = 5)


datos <- rnorm(n, mu,s)
hist(datos, freq = FALSE)
curve(rnorm(x, mu,s), col= "blue", lwd = 5,add = TRUE)



summary(datos)
quantile(datos, .99) #VaR con 99% de confianza 

x= seq(mu - 4*s, mu + 4*s , by = .5)
plot(ecdf(datos))
curve(pgamma(x,mu,s),xlim = c(0, mu + 4*s), col = "red" , add = TRUE)

distributions <- c("Beta",
                   "Binomial",
                   "Cauchy",
                   "Xi-Cuadrada",
                   "Exponencial",
                   "Distribución-F",
                   "Gamma",
                   "Geométrica",
                   "Hipergeómetrica",
                   "Lognormal",
                   "Multinomial",
                   "Binomial Negativa",
                   "Normal",
                   "Poisson",
                   "T Student",
                   "Uniforme",
                   "Weibull")
#dashboard de variables aleatorias 
#diseño 
ui<-fluidPage(
  titlePanel('Distribuciones'),
  theme = shinythemes::shinytheme('superhero'),
  sidebarLayout(
    sidebarPanel(
  selectInput('dist',label="Distribuciones",
              choices = distributions),
  sliderInput(inputId = "n",label="Número de Simulaciones",
              value=500,min = 1,max = 1000),
  sliderInput(inputId = "p1",label="Parametro 1",
              value=1,min=0,max=100),
  sliderInput(inputId = "p2",label="Parametro 2",
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
               plotOutput("g7"))),
  verbatimTextOutput("t1")
  )
  )
)

#carga de datos
server <- function(input,output){
  data <- reactive({
    if(input$dist == "Beta"){
      rbeta(input$n, input$p1, input$p2)}
    else if(input$dist == "Binomial"){
      rbinom(input$n, input$p1, input$p2)}
    else if(input$dist == "Cauchy"){
      rcauchy(input$n, input$p1, input$p2)}
    else if(input$dist == "Xi-Cuadrada"){
      rchisq(input$n, input$p1, input$p2)}
    else if(input$dist == "Exponencial"){
      rexp(input$n, input$p1)}
    else if(input$dist == "Distribución-F"){
      rf(input$n, input$p1, input$p2)}
    else if(input$dist == "Geométrica"){
      rgeom(input$n, input$p1)}
    else if(input$dist == "Hipergeómetrica"){
      rhyper(input$n, input$p1, input$p2)}###########
    else if(input$dist == "Lognormal"){
      rlnorm(input$n, input$p1, input$p2)}
    else if(input$dist == "Multinomial"){
      rmultinom(input$n, input$p1, input$p2)}
    else if(input$dist == "Binomial Negativa"){
      rnbinom(input$n, input$p1, input$p2)}#########
    else if(input$dist == "Normal"){
      rnorm(input$n, input$p1, input$p2)}
    else if(input$dist == "Poisson"){
      rpois(input$n, input$p1)}
    else if(input$dist == "T Student"){
      rt(input$n, input$p1)}
    else if(input$dist == "Uniforme"){
      runif(input$n, input$p1, input$p2)}
    else if(input$dist == "Weibull"){
      rweibull(input$n, input$p1, input$p2)}
    else if(input$dist == "Gamma"){
      rgamma(input$n, input$p1, input$p2)
    }
  })
  
  output$g1 <- renderPlot({
    plot(data())
  })
  
  output$g2 <- renderPlot({
    hist(data(), freq = FALSE, main = "Histograma",color= 'blue', border = 'white')
    if(input$dist== 'Beta'){
      curve( dbeta(x,input$p1, input$p2),col="blue",add = TRUE)
    }
    else if(input$dist=="Binomial"){
      curve(dbinom(x,input$p1,input$p2),col="green",add = TRUE)
    }
    else if(input$dist=="Cauchy"){
      curve(dcauchy(x,input$p1,input$p2),col="green",add = TRUE)
    }
    else if(input$dist=="Xi-Cuadrada"){
      curve(dchisq(x,input$p1,input$p2),col="green",add = TRUE)
    }
    else if(input$dist=="Exponencial"){
      curve(dexp(x,input$p1),col="blue",add = TRUE)
    }
    else if(input$dist=="Distribución F"){
      curve(df(x,input$p1,input$p2),col="green",add = TRUE)
    }
    else if(input$dist=="Gamma"){
      curve(dgamma(x,input$p1,input$p2),col="red",add = TRUE)
    }
    else if(input$dist=="Binomial"){
      curve(dbinom(x,input$p1,input$p2),col="green",add = TRUE)
    }
   
  })
  
  output$g3 <- renderPlot({
    plot(ecdf(data()))
    if(input$dist=="Exponencial"){
      curve(pexp(x,input$p1),col="red",
            main="Funci?n de Distribuci?n",add = TRUE)
      legend("topleft",lty = c(2,1),lwd = c(1,2),
             col=c("black","red"),legend = c("Emp?rica","Teorica"))  
    }
    if(input$dist=="Gamma"){
      curve(pgamma(x,input$p1,input$p2),col="red",
            main="Funci?n de Distribuci?n",add = TRUE)
      legend("topleft",lty = c(2,1),lwd = c(1,2),
             col=c("black","red"),legend = c("Emp?rica","Teorica"))  
    }
  })
  output$g4 <- renderPlot({
    boxplot(data())
  })
  output$t1 <- renderPrint({
    summary(data())
  })
  output$g5 <- renderPlot({
    if(input$dist=="Exponencial"){
    curve(dexp(x,input$p1),xlim = c(0,50),main="Densidad")  
  }
    if(input$dist=="Gamma"){
      curve(dgamma(x,input$p1,input$p2),xlim = c(0,50),main="Densidad")  
    }
    if(input$dist=="Beta"){
      curve(dbeta(x,input$p1,input$p2),xlim = c(-1,2),main="Densidad")  
    }
  })
  output$g6 <- renderPlot({
    if(input$dist=="Exponencial"){
      curve(pexp(x,input$p1),xlim = c(0,50),main="Distribuci?n")
    }
    else if(input$dist=="Gamma"){
      curve(pgamma(x,input$p1,input$p2),xlim = c(0,50),main="Distribuci?n")  
    }
  })
  output$g7 <- renderPlot({
    if(input$dist=="Exponencial"){
      curve(1-pexp(x,input$p1),xlim = c(0,50),main="Sobrevivencia")
    }
    else if(input$dist=="Gamma"){
      curve(1-pgamma(x,input$p1,input$p2),xlim = c(0,50),main="Sobrevivencia")  
    }
  })
}

#Publicación del tablero
shinyApp(ui = ui, server = server)

