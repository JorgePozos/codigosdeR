library(babynames)
library(shiny)
library(dplyr)
library(ggplot2)
#nombres populares
trendy_names<- babynames %>%
  filter(year== 2000)%>%
  arrange(desc(prop))%>%
  top_n(10)

ui <- fluidPage(
  #seleccion del año 
  sliderInput("año", "label",
              value = 1980,
              min= 1880,
              max = 2010),
  #selección de nombre
  textInput("name", "Enter a name:","Jorge"),
  # CODE BELOW: Add a plotly output named 'plot_trendy_names'
  plotly::plotlyOutput('plot_trendy_names')
)
server <- function(input, output, session){
  # Function to plot trends in a name
  plot_trends <- function(){
    babynames %>% 
      filter(name == input$name) %>% 
      ggplot(aes(x = year, y = n)) +
      geom_col()
  }
  # CODE BELOW: Render a plotly output named 'plot_trendy_names'
  output$plot_trendy_names <- plotly::renderPlotly({
    plot_trends()
  })
}
shinyApp(ui = ui, server = server)
