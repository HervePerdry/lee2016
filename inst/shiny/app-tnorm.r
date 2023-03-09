library(shiny)
# a list called 'global' must be defined in the session !
# with components sd, low, upp, x0

if(!exists("global"))
  global <- list(x0 = 0.9, low = -Inf, upp = +Inf, sd = 1)
 
CI <- round(conf.int.tnorm( global$x0, alpha = 0.05, sd = global$sd, low = global$low, upp = global$upp ),2)

ui <- fluidPage(
  titlePanel("Confidence interval for truncated Gaussian"),
  verticalLayout(
    mainPanel(
      plotOutput(outputId = "distPlot"),      
      sliderInput(inputId = "mu", label = "mean:", min = CI[1], max = CI[2], value = global$x0, step = 0.01, width = "100%")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    demo.tnorm(global$x0, input$mu, global$sd, global$low, global$upp, CI)
  })

}

shinyApp(ui = ui, server = server)
