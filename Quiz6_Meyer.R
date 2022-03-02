## “All work presented is my own, and I have followed all rules for collaboration.”
## Trent Meyer

## My app used the pokemon_df data set, and prompted the user to choose a type, 
## along with a variable from a list of quantitative variables I chose. Then,
## a boxplot is created for that type of Pokemon with the variable they chose.
library(shiny)
library(tidyverse)
library(billboard)
library(rvest)

pokemon_df <- read_csv("data/pokemon_allgen.csv") %>%
  mutate(Generation_cat = factor(Generation))

var_choices <- names(pokemon_df)[c(6:11, 16, 17, 18)]

ggplot(data = pokemon_df, aes(x = Attack)) +
  geom_boxplot()

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons("varchoice", label = "Choose a variable", 
                     choices = var_choices),
      selectInput("typechoice", label = "Choose a type",
                  choices = pokemon_df$`Type 1`)),
    mainPanel(plotOutput("boxplot")
    )  
  ))


server <- function(input, output, session) {

  df_sub <- reactive({
    pokemon_df %>%
      filter(`Type 1` == input$typechoice)
  })
  
  boxplot <- reactive({
    ggplot(data = df_sub(), aes(x = .data[[input$varchoice]])) +
      geom_boxplot() +
      coord_flip() 
  })
  
  output$boxplot <- renderPlot({
    boxplot()
  })
}

shinyApp(ui, server)