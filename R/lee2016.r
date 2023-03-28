#' Lee 2016 paper
#' @details Runs a presentation of Lee 2016 paper with embedded shiny apps.
#' @export
lee2016 <- function() rmarkdown::run(system.file("lee2016.Rmd", package = "lee2016"))
