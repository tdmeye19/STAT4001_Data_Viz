library(shiny)

# Exercise 3.3.6 1:
ui <- fluidPage(
textInput("name", "What's your name?"),
textOutput("greeting")
)

## This one you needed to switch from input to output, and 
## indicate input$name so you get what the ui has for their name
# server <- function(input, output, server) {
# output$greeting <- renderText(paste0("Hello ", input$name))
# }

## needed to have a reactive expression around paste0
# server2 <- function(input, output, server) {
# greeting <- reactive(paste0("Hello ", input$name))
# output$greeting <- renderText(greeting())
# }


## This one has greeting mispelled
# server3 <- function(input, output, server) {
# output$greeting <- renderText({
# paste0("Hello ", input$name)
# })
# }

shinyApp(ui, server)

# Exercise 3.3.6 2: Draw the reactive graph for the following server functions:
## I emailed photos of these to Professor Higham.
