library(shiny)
library(learnr)

ui <- fluidPage(
  learnr::run_tutorial(
    name = "tutorial.Rmd",   # FILE MODE (not directory)
    package = NULL
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
