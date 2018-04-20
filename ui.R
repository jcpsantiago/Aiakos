#
# This is the Hopper Stopper user-interface Shiny definition
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),
  theme = shinytheme("cosmo"),
  br(),
  # Application title
  titlePanel("Hopper Stopper",
             windowTitle = "Hopper Stopper"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 2,
                 ## display logo
                 imageOutput("logo"),
                 br(), br()
    ),
    
    # main panel with tabs
    mainPanel(width = 10,
                tabsetPanel(
                   ## form for adding new participants
                   tabPanel(icon = icon("user"), "Participants",
                            br(),
                            textInput(inputId = "first_name", label = "First name", value = ""),
                            textInput(inputId = "last_name", label = "Last name"),
                            dateInput(inputId = "date_of_birth", 
                                      label = "Date of birth", 
                                      value = "1990-01-01",
                                      format = "yyyy-mm-dd", 
                                      startview = "year",
                                      language = "en"),
                            selectInput("study_name", "Select study",
                                        c("", "InsuSO", "THC", "Ghremory")),
                            actionButton(inputId = "add_part",
                                         label = "Submit", icon = icon("check"))),
                   
                   
                   tabPanel(icon = icon("plus"), "Add study",
                            br(),
                            div(id = "add_study",
                            textInput(inputId = "title", label = "Title of the study", value = ""),
                            textInput(inputId = "main_res", label = "Main researcher"),
                            selectInput(inputId = "task",
                                        label = "Select tasks",
                                        choices = c("MDBF", "PVT", "SF-A-R"),
                                        selected = "",
                                        selectize = TRUE,
                                        multiple = TRUE)),
                            hidden(textInput("date_added", "")),
                            actionButton(inputId = "submit_study",
                                         label = "Submit", icon = icon("check"))),
                   
                   
                   tabPanel(icon = icon("plus"), "Add task",
                            textInput(inputId = "task_name", 
                                      label = "Task name"),
                            textInput(inputId = "task_desc",
                                      label = "Short description")),
                   
                   
                   tabPanel(icon = icon("clipboard"), "Tasks"),
                   
                   
                   tabPanel(id = "view_studies", icon = icon("flask"), "Studies",
                            br(),
                            DT::dataTableOutput("studies_table"))
                   )
    )
  )
)
)
