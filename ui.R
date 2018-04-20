#
# This is the Hopper Stopper user-interface Shiny definition
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

library(shiny)
library(shinyjs)
library(shinythemes)

source("storage.R")

studies <- load_data("studies")

tasks <- load_data("tasks")

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
                            div(id = "add_participant",
                            textInput(inputId = "first_name", label = "First name", value = ""),
                            textInput(inputId = "last_name", label = "Last name"),
                            dateInput(inputId = "date_of_birth", 
                                      label = "Date of birth", 
                                      # value = "1990-01-29",
                                      format = "yyyy-mm-dd", 
                                      startview = "year",
                                      language = "en"),
                            selectInput("study_name", "Select study",
                                        levels(as.factor(studies$title)))),
                            hidden(textInput("date_added", "")),
                            actionButton(inputId = "add_part_click",
                                         label = "Submit", icon = icon("check"))),
                   
                   
                   tabPanel(icon = icon("plus"), "Add study",
                            br(),
                            div(id = "add_study",
                            textInput(inputId = "title", label = "Title of the study", value = ""),
                            textInput(inputId = "main_res", label = "Main researcher"),
                            selectInput(inputId = "date_started", 
                                        label = "Year started",
                                        choices = seq(from = 2016, to = 2100, by = 1),
                                        selected = 2018),
                            selectInput(inputId = "tasks",
                                        label = "Select tasks",
                                        choices = levels(as.factor(tasks$name)),
                                        selected = "",
                                        selectize = TRUE,
                                        multiple = TRUE)),
                            hidden(textInput("date_added", "")),
                            actionButton(inputId = "add_study_click",
                                         label = "Submit", icon = icon("check"))),
                   
                   
                   tabPanel(icon = icon("plus"), "Add task",
                            textInput(inputId = "task_name", 
                                      label = "Task name"),
                            textInput(inputId = "task_desc",
                                      label = "Short description"),
                            actionButton(inputId = "add_task_click",
                                         label = "Submit", icon = icon("check"))),
                   
                   
                   tabPanel(icon = icon("clipboard"), "Tasks",
                            br(),
                            DT::dataTableOutput("tasks_table")),
                   
                   tabPanel(id = "view_studies", icon = icon("flask"), "Studies",
                            br(),
                            DT::dataTableOutput("studies_table"))
                   )
    )
  )
)
)
