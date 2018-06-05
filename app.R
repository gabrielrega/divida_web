library(shiny)

# Define UI para o app que desenha o gráfico de barras
ui <- fluidPage(
   
   # Application title
   titlePanel("Equilibrando a dívida pública"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("debt",
                     "Dívida Inicial (% do PIB):",
                     min = 10,
                     max = 200,
                     value = 70),
         sliderInput("rate",
                     "Taxa de Juros real média paga (%):",
                     min = -5,
                     max = 20,
                     value = 7),
         sliderInput("growth",
                     "Crescimento real do PIB (%):",
                     min = -10,
                     max = 15,
                     value = 0),
         sliderInput("surplus",
                     "Superávit Nominal (% do PIB):",
                     min = -10,
                     max = 10,
                     value = 3)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot2"),
         textOutput("buiter"),
         textOutput("conta"),
         textOutput("prim")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    z <- vector(mode = "integer", length = 20)
    y <- vector(mode = "integer", length = 20)
    y[1] <- 100
    z[1] <- input$debt
    for (i in 2:23) {
      z[i] <- z[i-1] + (z[i-1] * input$rate/100) - (y[i-1] * input$surplus/100)
      y[i] <- y[i-1] + (y[i-1] * input$growth/100)
    }
    
    d <- 100*z/y
    names(d) <- seq(18,40)
    
    # draw the histogram with the specified number of bins
    barplot(d)
    
  })
  
  output$buiter <- renderText({
      paste("Superávit Nominal Necessário:",
          round(100*(input$debt/100 * (input$rate/100 - input$growth/100) / (1 + input$growth/100)), digits = 2),
            " % do PIB")
  })

  output$conta <- renderText({
    paste("Conta de Juros:",
          100*(input$debt/100 * input$rate/100),
          " % do PIB")
  })  
  
  output$prim <- renderText({
    paste("Superávit Primário Equivalente:",
          round(100*(input$debt/100 * (input$rate/100 - input$growth/100) / (1 + input$growth/100)), digits = 2) + 100*(input$debt/100 * input$rate/100),
          " % do PIB")
  })

}
# Run the application 
shinyApp(ui = ui, server = server)

