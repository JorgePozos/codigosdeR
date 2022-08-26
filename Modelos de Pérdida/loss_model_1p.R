ui<-fluidPage(
  selectInput('dist',label="Distribuciones",
              choices = c("Exponencial","Gamma")),
  sliderInput(inputId = "n",label="Número de Simulaciones",
              value=100,min = 1,max = 500),
  sliderInput(inputId = "p1",label="Parametro 1",
              value=1,min=0,max=100),
  sliderInput(inputId = "p2",label="Parametro 2",
              value=1,min=0,max=100),
  plotOutput("g1"),
  plotOutput("g2")
)
#Carga de los datos
server<-function(input,output){
  data<-reactive({
    rexp(input$n,input$p1)
  })
  
  output$g1<-renderPlot({
    hist(data(),freq = FALSE,main = "Histograma")
    curve(dexp(x,input$p1),col="gold",add = TRUE)
  })
  output$g2 <- renderPlot({
    plot(ecdf(data()))
    curve(pexp(x,input$p1),col="purple",
          main="Función de distribución",add=TRUE)
    legend("topleft",lty=c(2,1),lwd = c(1,2),
           col =c("blue","pink"),legend = c("Empírica","Teórica"))
  })
}

#Publicación del tablero
shinyApp(ui=ui,server = server)