library(shiny)
library(shinyjs)
library(shinythemes)
library(pool)
library(RSQLite)
library(dplyr)
library(purrr)

source("helper.R")
source("storage.R")

pool <- dbPool(RSQLite::SQLite(), dbname = "hopper.db")

onStop(function() {
  poolClose(pool)
})



#### Define UI for application ####
ui <- shinyUI(fluidPage(
    
    useShinyjs(),
    theme = shinytheme("cosmo"),
    inlineCSS(appCSS),
    br(),
    # Application title
    titlePanel(div("Hopper Stopper",
               h4("Institute of Medical Psychology")),
               windowTitle = "Hopper Stopper"),
    
    # Sidebar  
    sidebarLayout(
      sidebarPanel(width = 2,
                     div(style="text-align:center",
                         tags$p(a(icon("github", "fa-2x"), href = "https://github.com/jcpsantiago/hopperStopper"),
                           a(icon("envelope", "fa-2x"), href="mailto:joao.santiago@uni-tuebingen.de?Subject=Help!%20HopperStopper%20not%20working!", target="_top")),
                           br(), br())
                   ),
      
      # main panel with tabs
      mainPanel(width = 10,
                tabsetPanel(
                  ## form for adding new participants
                  tabPanel(icon = icon("user"), "Participants",
                           div(id = "add_participant",
                               textInput(inputId = "first_name", label = "First name"),
                               textInput(inputId = "last_name", label = "Last name"),
                               hidden(div(id = "part_test_info",
                                          p(strong("WARNING:"), "This person has taken part in
                                            other studies!", emo::ji("hand")),
                                          p("Make sure there are no conflicts between studies."),
                                          tableOutput(outputId = "part_test"))),
                               dateInput(inputId = "date_of_birth", 
                                         label = "Date of birth", 
                                         value = "1990-01-29",
                                         format = "yyyy-mm-dd", 
                                         startview = "year",
                                         language = "en")),
                           selectInput("study_title_sel", "Select study",
                                       levels(as.factor(pool %>% 
                                                          tbl("studies") %>% 
                                                          select(study_title) %>%
                                                          pull()
                                                        )
                                              )
                                       ),
                           hidden(textInput("date_added", "")),
                           actionButton(inputId = "add_part_click",
                                        label = "Submit", icon = icon("check"))),
                  
                  
                  tabPanel(icon = icon("plus"), "Add study",
                           div(id = "add_study",
                               textInput(inputId = "study_title", 
                                         label = "Title of the study", 
                                         value = ""),
                               textInput(inputId = "contact_person", 
                                         label = "Name of the contact person"),
                               selectInput(inputId = "year_started", 
                                           label = "Year started",
                                           choices = seq(from = 2014, to = 2100, by = 1),
                                           selected = 2018),
                               selectInput(inputId = "tasks",
                                           label = "Select tasks",
                                           choices = levels(as.factor(pool %>%
                                                                        tbl("tasks") %>% 
                                                                        select(task_name) %>%
                                                                        pull()
                                                                      )
                                                            ),
                                           selected = "",
                                           selectize = TRUE,
                                           multiple = TRUE)),
                           hidden(textInput("date_added", "")),
                           actionButton(inputId = "add_study_click",
                                                 label = "Submit", icon = icon("check"))),
                  
                  
                  tabPanel(icon = icon("plus"), "Add task",
                           div(id = "add_task",
                               textInput(inputId = "task_name", 
                                         label = "Task name",
                                         value = ""),
                               hidden(div(id = "task_check_error",
                                          p(strong("This task is already in the database."), utf8::utf8_print(intToUtf8(0x1f605))),
                                          p("See below if it's the same, otherwise choose another name."),
                                          tableOutput(outputId = "task_test_table"),
                                          br())),
                               textInput(inputId = "task_desc",
                                         label = "Short description")),
                           disabled(actionButton(inputId = "add_task_click",
                                        label = "Submit", icon = icon("check")))),
                  
                  
                  tabPanel(icon = icon("clipboard"), "Tasks",
                           DT::dataTableOutput("tasks_table")),
                  
                  tabPanel(id = "view_studies", icon = icon("flask"), "Studies",
                           DT::dataTableOutput("studies_table"))
                  )
                  )
  )
  )
  )



# Define server logic required to draw a histogram
#
# This is the Hopper Stopper server logic
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

#### Define server logic required to draw a histogram ####
server <- shinyServer(function(input, output, session) {
  
  updateTextInput(session, "date_added", value = get_time_human())
  selected_tasks <- reactive(input$tasks)
  study_title <- reactive(input$study_title)
  
  # output$logo <- renderImage({
  #   list(src = here::here("figs/logo.svg"),
  #        height = "200px",
  #        contentType = "image/svg+xml")
  # }, deleteFile = FALSE)
  
  #### Add participant ####

  df_part <- reactive({
    if (input$first_name > 0 && input$last_name > 0){
      pool %>% tbl("participants") %>%
        filter(first_name == stringr::str_extract(openssl::sha256(tolower(input$first_name)), "[0-9a-z]+") & last_name == stringr::str_extract(openssl::sha256(tolower(input$last_name)), "[0-9a-z]+")) %>%
        collect
    } else {
      NULL
    }
  })
  
  df_part_joined <- reactive({
    if(!(is.null(df_part())) && nrow(df_part()) > 0){
      df_part() %>%
        left_join(., pool %>% tbl("part_study"), by = c("id" = "part_reference"), copy = TRUE) %>%
        left_join(., pool %>% tbl("study_task"), by = "study_reference", copy = TRUE) %>%
        left_join(., pool %>% tbl("studies"), by = c("study_reference" = "id"), copy = TRUE) %>%
        left_join(., pool %>% tbl("tasks"), by = c("task_reference" = "id"), copy = TRUE) %>%
        select(c(study_title, year_started, task_name)) %>%
        arrange(task_name) %>%
        distinct() %>%
        group_by(study_title, year_started) %>%
        summarize(tasks = stringr::str_flatten(task_name, collapse = ", ")) %>%
        rename("Study title" = study_title,
               "Year study started" = year_started,
               "Tasks" = tasks)
    } else {
      NULL
    }
  })
  
  observe({
    output$part_test <- renderTable(df_part_joined())
  })
  
  observe({
    toggleElement("part_test_info", condition = !(is.null(df_part_joined())))
    toggleState("add_part_click", condition = (!(is.null(df_part())) && is.null(df_part_joined())))
  })

  part_data <- reactive({
    map_df(fields_participant, ~ input[[.]])
  })
  
  # When the Submit button is clicked 
  observeEvent(input$add_part_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    # save_data(part_data(), "participants")
    save_data_tidy(pool, part_data(), "participants")
    map_part_study(input$study_title_sel)
    
    reset("add_participant")
  })
  
  
  #### Add task ####
  # Gather all the inputs and format tasks as a nice string
  task_data <- reactive({
    l <- sapply(fields_task, function(x) x = input[[x]])
    names(l) <- c("task_name", "description")
    l
  })
  
  
  ## disable submit button, while there is no input, or
  ## if the task already exists in the DB
  # check if the given task name already exists in the db
  ts_test <- reactive({
    ts <- pool %>% tbl("tasks") %>% select(task_name) %>% pull() %>%
      map_chr(., ~ lower_squish_str(.))
    
    lower_squish_str(input$task_name) %in% ts
  })
  
  observe({
    if(!is.null(input$task_name) & input$task_name > 0){
      # ts <- pool %>% tbl("tasks") %>% select(task_name) %>% pull() %>%
      #   map_chr(., ~ lower_squish_str(.))
      # 
      # ts_test <- lower_squish_str(input$task_name) %in% ts

      toggleState("add_task_click",
                  condition = !(ts_test()))
      toggle("task_check_error",
                  condition = ts_test())
    } else
      disable("add_task_click")
      # hide("task_check_error")
  })
  
  observe({
    if(ts_test()){
      output$task_test_table <- renderTable({
        lower_case_task_name <- lower_squish_str(input$task_name)
        
        pool %>% tbl("tasks") %>% 
          select(task_name, description) %>%
          collect %>%
          mutate(task_name = map_chr(task_name, ~ lower_squish_str(.))) %>%
          filter(task_name == lower_case_task_name) %>%
          mutate(task_name = input$task_name) %>%
          rename("Task name" = task_name,
                 "Description" = description)
      })
    }
  })
  
  
  # When the Submit button is clicked 
  observeEvent(input$add_task_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(task_data(), "tasks")
    
    reset("add_task")
    
    updateSelectInput(session, "tasks", 
                      choices = levels(as.factor(pool %>%
                                                   tbl("tasks") %>% 
                                                   select(task_name) %>%
                                                   pull()
                      )
                      )
    )
    
  })
  
  # Update the studies whenever a new submission is made
  tasks_datatable <- reactive({
    input$add_task_click
    pool %>% tbl("tasks") %>% select(-c(id, date_added)) %>% arrange(task_name) %>%
      collect
  })
  
  # Show the studies in a table
  output$tasks_table <- DT::renderDataTable(
    DT::datatable(
      tasks_datatable(),
      rownames = FALSE, colnames = c("Task name", "Description"), style = "bootstrap",
      options = list(searching = TRUE, lengthChange = TRUE, scroller = TRUE)
    )
  )
  
  
  #### Add study ####
  # Gather all the inputs and format tasks as a nice string
  studies_data <- reactive({
    sapply(fields_study, function(x) x = input[[x]])
  })
  
  # When the Submit button is clicked 
  observeEvent(input$add_study_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(studies_data(), "studies")
    
    # map study to tasks
    purrr::walk(selected_tasks(), ~ map_study_task(study_title(), .))
    
    reset("add_study")
    
    updateSelectInput(session, "study_title_sel", 
                      choices = levels(as.factor(pool %>%
                                                   tbl("studies") %>% 
                                                   select(study_title) %>%
                                                   pull()
                      )
                      )
    )
  })
  
  # Update the studies whenever a new submission is made
  studies_datatable <- reactive({
    input$add_study_click
    load_data(table = "studies")
  })
  
  # Show the studies in a table
  output$studies_table <- DT::renderDataTable(
    DT::datatable(
      studies_datatable() %>% dplyr::select(-id),
      rownames = FALSE, style = "bootstrap",
      colnames = c("Title", "Main researcher", "Date started", "Date added"),
      options = list(searching = TRUE, lengthChange = TRUE, scrollX = TRUE)
    )
  )
  
})


shinyApp(ui, server)
