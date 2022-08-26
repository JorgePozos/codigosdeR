
#### Aplicaci?n 1. Variables aleatorias ####

# Librerias de variables aleatorias
library(actuar)
library(stats)
help(distributions)

# An?lisis de Variables Aleatorias

# Normal 
n=100000
mu=3
s=5
x=seq(mu-4*s,mu+4*s,by=0.5)
curve(dnorm(x,mu,s),xlim=c(mu-4*s,mu+4*s),col="blue",lwd=2)
curve(pnorm(x,mu,s),xlim=c(mu-4*s,mu+4*s),col="blue",lwd=2)
curve(1-pnorm(x,mu,s),xlim=c(mu-4*s,mu+4*s),col="blue",lwd=2)

datos=rnorm(n,mu,s)
hist(datos,freq=FALSE)
summary(datos)
quantile(datos,0.95) # VaR con un nivel de confianza del 99%

x=seq(0,mu+4*s,by=0.5)
plot(ecdf(datos)) # datos emp?ricos
curve(pgamma(x,mu,s),xlim = c(0,mu+4*s),col="red",
      add = TRUE) # datos teorico


# Dashboard de variables aleatorias

library(shiny)

#Dise?o del Tablero 
ui<-fluidPage(
  selectInput('dist',label="Distribuciones",
              choices = c("Exponencial","Gamma")),
  sliderInput(inputId = "n",label="N?mero de Simulaciones",
              value=10,min = 1,max = 1000),
  sliderInput(inputId = "p1",label="Parametro 1",
              value=1,min=0,max=100),
  sliderInput(inputId = "p2",label="Parametro 2",
              value=1,min=0,max=100),
  mainPanel(
    tabsetPanel(
      tabPanel('Datos',
        plotOutput("g1")), 
      tabPanel('Histograma',
               plotOutput("g2")),
      tabPanel('Empirica vs Teórica',
               plotOutput("g3")),
      tabPanel('Vela',
               plotOutput("g4")),
      tabPanel('Estadística Descriptiva',
               verbatimTextOutput("t1")), 
      tabPanel('Densidad',
               plotOutput("g5")),
      tabPanel('Distribución',
               plotOutput("g6")),
      tabPanel('Sobrevivencia',
               plotOutput("g7")),
    ) 
  ) 
)

#Carga de los datos
server<-function(input,output){
  data<-reactive({
    if(input$dist=="Exponencial"){
      rexp(input$n,input$p1)
    }
    else{
      rgamma(input$n,input$p1,input$p2)
    }
    
  })
  
  output$g1<-renderPlot({
    plot(data())
  })
  
  output$g2 <- renderPlot({
    hist(data(),freq = FALSE,main="Histograma")
    if(input$dist=="Exponencial"){
      curve(dexp(x,input$p1),col="blue",add = TRUE)
    }
    if(input$dist=="Gamma"){
      curve(dgamma(x,input$p1,input$p2),col="blue",add = TRUE)
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
  
  output$g4 <-renderPlot({
    boxplot(data())
  })
  output$t1 <- renderPrint({
    summary(data())
  })
  output$g5<-renderPlot({
    if(input$dist=="Exponencial"){
      curve(dexp(x,input$p1),xlim = c(0,1),main="Densidad")  
    }
    if(input$dist=="Gamma"){
      curve(dgamma(x,input$p1,input$p2),xlim = c(0,1),main="Densidad")  
    }
  })
  output$g6<-renderPlot({
    if(input$dist=="Exponencial"){
      curve(pexp(x,input$p1),xlim = c(0,1),main="Distribuci?n")
    }
    if(input$dist=="Gamma"){
      curve(pgamma(x,input$p1,input$p2),xlim = c(0,1),main="Distribuci?n")  
    }
  })
    
  output$g7<-renderPlot({
    if(input$dist=="Exponencial"){
      curve(1-pexp(x,input$p1),xlim = c(0,1),main="Sobrevivencia")
    }
    if(input$dist=="Gamma"){
      curve(1-pgamma(x,input$p1,input$p2),xlim = c(0,1),main="Sobrevivencia")  
    }
  })
}
  
#Publicación del Tablero
shinyApp(ui=ui,server=server)







