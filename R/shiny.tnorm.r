#' Shiny demo for truncated normal confidence interval
#'
#' @export
shiny.tnorm <- function(x0 = 0, low = -Inf, upp = Inf, sd = 1) {

  CI <- round(conf.int.tnorm( x0, alpha = 0.05, sd = sd, low = low, upp = upp ),2)

  ui <- fluidPage(
    titlePanel("Confidence interval for truncated Gaussian"),
    verticalLayout(
      mainPanel(
        plotOutput(outputId = "distPlot"),
        sliderInput(inputId = "mu", label = "mean:", min = CI[1], max = CI[2], value = x0, step = 0.01, width = "100%")
      )
    )
  )

  server <- function(input, output) {
    output$distPlot <- renderPlot(demo.tnorm(x0, input$mu, sd, low, upp, CI))
  }

  shinyApp(ui = ui, server = server)
}

