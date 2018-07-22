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
                  value = 70,
                  step = 5),
      sliderInput("rate",
                  "Taxa de Juros real média paga (%):",
                  min = -5,
                  max = 20,
                  value = 7,
                  step = 0.25),
      sliderInput("growth",
                  "Crescimento real do PIB (%):",
                  min = -10,
                  max = 15,
                  value = 0,
                  step = 0.25),
      sliderInput("surplus",
                  "Superávit Primário (% do PIB):",
                  min = -10,
                  max = 10,
                  value = 3,
                  step = 0.25),
      
      
      
      tags$div(class="header", checked=NA,
               tags$p("Conheça meus outros trabalhos!"),
               tags$a(href="https://gabrielrega.com/", "Visite meu blog!"),
               tags$p(" "),
               tags$a(href="https://twitter.com/gabrielrega", "Me siga no Twitter!"),
               tags$p(" "),
               tags$p("Feito por Gabriel Rega (2018)")
      )
      
               
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot2"),
      h3("Instruções"),
      p("Existem muitos mitos em relação à dívida pública. O primeiro deles é que ela deve ser paga! Outros acreditam que existe um nível ideal dela e que devemos ter o objetivo de caminhar na direção deste. Nossa ideia aqui é que a dívida é um recurso útil que o governo deve utilizar quando for necessário. O objetivo do webapp é mostrar como é possível equilibrar o crescimento da dívida, mudando as variáveis econômicas subjacentes."),
      p("Mudando os valores nos sliders correspondentes às variáveis, o gráfico de crescimento da dívida vai se ajustando."),
      p("Dívida inicial representa em quanto a dívida começa em 2018."),
      p("Taxa de juros reais média paga representa o percentual que é cobrado em média sobre a dívida existente, descontada a inflação."),
      p("Crescimento real do PIB é quanto o PIB aumenta de um ano para o outro, sem contar a inflação."),
      p("Superávit primário é o quanto o governo arrecada a mais do que gasta em um ano. Um valor negativo representa que o governo gasta mais do que arrecada."),
      
      textOutput("conta"),
      textOutput("nomi"),
      textOutput("buiter")
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
    #surplus <- (input$surplus) - (100*(input$debt/100 * input$rate/100))
    
    for (i in 2:23) {
      z[i] <- z[i-1] + (z[i-1] * input$rate/100) - (y[i-1] * input$surplus/100)
      y[i] <- y[i-1] + (y[i-1] * input$growth/100)
    }
    
    d <- 100*z/y
    names(d) <- seq(2018,2040)
    
    # draw the histogram with the specified number of bins
    barplot(d, 
            ylab = "% do PIB",
            main = "Relação Dívida/PIB",
            las = 2)
    
  })
  
  output$buiter <- renderText({
    paste("O superávit nominal necessário para equilibrar a dívida com os parâmetros escolhidos é de",
          round(100*(input$debt/100 * (input$rate/100 - input$growth/100) / (1 + input$growth/100)), digits = 2),
          " % do PIB")
  })
  
  output$conta <- renderText({
    paste("Com os parâmetros selecionados, o país está pagando anualmente de juros",
          100*(input$debt/100 * input$rate/100),
          " % do PIB.")
  })  
  
  output$nomi <- renderText({
    paste("Somando o superávit primário escolhido à esta conta de juros chegamos no valor de superávit nominal de",
          input$surplus - (100*(input$debt/100 * input$rate/100)),
          " % do PIB")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
