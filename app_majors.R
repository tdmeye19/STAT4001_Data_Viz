library(tidyverse)
library(readxl)
df <- read_excel("data/slu_graduates_17_21.xlsx")

## fixes error in the data
df <- df %>% mutate(across(everything(),
                           .fns = ~replace(., . ==  "STATS" , "STAT")))

df_long <- df %>% pivot_longer(3:8, names_to = "type", values_to = "discipline")
df_major <- df_long %>% 
  filter(type == "major1" | type == "major2" | type == "major3")

df_stat <- df_major %>% filter(discipline == "STAT") 
df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
  filter(type == "major1" |
           type == "major2" | 
           type == "major3") 

df_nostat <- df_statfull %>% filter(discipline != "STAT" &
                                      !is.na(discipline)) %>%
  group_by(discipline) %>%
  summarise(nstudent = n()) %>%
  mutate(discipline = fct_reorder(discipline, nstudent)) 
ggplot(data = df_nostat, aes(x = discipline, y = nstudent)) +
  geom_col() +
  coord_flip()


df_mcss <- df_major %>% filter(!is.na(discipline)) %>%
  mutate(discipline = factor(discipline)) 
  
df_mcss_new <- df_mcss %>% filter(discipline == "STAT" | discipline == "MATH" | discipline == "CS")
majors <- levels(df_mcss_new$discipline) [c(18, 45, 58)]

## exercise 3
ui <- fluidPage(
  sidebarLayout(
    radioButtons("majorchoice", label = "Choose a Major", choices = majors),
    mainPanel(plotOutput("m_plot"),
              tableOutput("gendertable"))
  )
)

server <- function(input, output, session) {
  
  
  major_reactive_df <- reactive({
    
    df_stat <- df_major %>% filter(discipline == input$majorchoice) 
    #df_statfull <- semi_join(df_long, df_stat, by = "adm_id") %>%
      #filter(type == "major1" |
               #type == "major2" | 
               #type == "major3")
    
    df_nostat <- df_statfull %>% filter(discipline != input$majorchoice &
                                          !is.na(discipline)) %>%
      group_by(discipline, sex) %>%
      summarise(nstudent = n()) %>%
      mutate(discipline = fct_reorder(discipline, nstudent))
  })
  
  
  
  major_plot <- reactive({
    ggplot(data = major_reactive_df(), aes(x = discipline, y = nstudent)) +
      geom_col() +
      coord_flip()
  })
  
  gendertable <- reactive({
    
  
  }) 
    
  output$m_plot <- renderPlot({
    major_plot()
  })
  
  ## Exercise 4
  output$gendertable <- renderTable(
    major_reactive_df(), options = list(pageLength = 5)
  )
}

shinyApp(ui, server)