#' Shiny demo for polyhedral conditions
#'
#' @export
shiny.polyhedra <- function(x, y, lambda.max = 100) {

  if(missing(x) | missing(y)) {
    cat("*** Using an example data set ***\n")
    x <- cbind( c(rep(-1, 20), rep(1,20)), runif(40, -1, 1) )
    x[,2] <- x[,2] + 0.4 * x[,1] 
    y <- x %*% c(1,2) + rnorm(40)
    y <- y - mean(y)
  }
  ui <- fluidPage(
    titlePanel("Lasso and polyhedral condiditions"),
    verticalLayout(
      mainPanel(
        plotOutput(outputId = "distPlot"),
        sliderInput(inputId = "lambda", label = "lambda:", min = 0, max = lambda.max, value = lambda.max/4, step = lambda.max/100, width = "100%")
      )
    )
  )

  server <- function(input, output) {
    output$distPlot <- renderPlot(demo.polyhedra(x, y, input$lambda))
  }

  shinyApp(ui = ui, server = server)
}

