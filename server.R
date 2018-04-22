#
# This is the Hopper Stopper server logic
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

library(shiny)
library(shinyjs)
library(magrittr)
library(pool)
library(RSQLite)
library(dplyr)
library(purrr)

source("helper.R")
source("storage.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  updateTextInput(session, "date_added", value = get_time_human())
  selected_tasks <- reactive(input$tasks)
  study_title <- reactive(input$study_title)
  
  # output$logo <- renderImage({
  #   list(src = here::here("figs/logo.svg"),
  #        height = "200px",
  #        contentType = "image/svg+xml")
  # }, deleteFile = FALSE)
  
#### Add participant ####
  # Gather all the inputs and format tasks as a nice string
  # part_data <- reactive({
  #   l <- sapply(fields_participant, function(x) x = input[[x]])
  #   l[["first_name"]] <- openssl::blake2b(tolower(l[["first_name"]]))
  #   l[["last_name"]] <- openssl::blake2b(tolower(l[["last_name"]]))
  #   l[["date_of_birth"]] <- lubridate::as_date(l[["date_of_birth"]])
  #   names(l) <- c("first_name", "last_name", "date_of_birth", "date_added")
  #   l
  # })
  
  observe({
      if (input$first_name > 0 & input$last_name > 0){
        df_part <- pool %>% tbl("participants") %>%
          filter(first_name == stringr::str_extract(openssl::blake2b(tolower(input$first_name)), "[0-9a-z]+") & last_name == stringr::str_extract(openssl::blake2b(tolower(input$last_name)), "[0-9a-z]+")) %>%
          collect
        
        parcheck <- count(df_part)
        
        if(parcheck$n > 0){
          
         a <- df_part %>%
            left_join(., pool %>% tbl("part_study"), by = c("id" = "part_reference"), copy = TRUE) %>%
            left_join(., pool %>% tbl("study_task"), by = "study_reference", copy = TRUE) %>%
            left_join(., pool %>% tbl("studies"), by = c("study_reference" = "id"), copy = TRUE) %>%
            left_join(., pool %>% tbl("tasks"), by = c("task_reference" = "id"), copy = TRUE) %>%
            select(c(study_title, year_started, task_name)) %>%
            arrange(task_name) %>%
           distinct() %>%
            group_by(study_title, year_started) %>%
            summarize(tasks = str_flatten(task_name, collapse = ", ")) %>%
            rename("Study title" = study_title,
                   "Year study started" = year_started,
                   "Tasks" = tasks)
          
          output$part_test <- renderTable(
            as.data.frame(a))
          
          toggle("part_test_info")
          toggle("part_test")
          
        } else {
          hide("part_test_info")
          hide("part_test")
        }
      } else {
        hide("part_test_info")
        hide("part_test")
      }
  })
  # 
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
  
  # When the Submit button is clicked 
  observeEvent(input$add_task_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(task_data(), "tasks")
    
    reset("add_task")
  })
  
  # Update the studies whenever a new submission is made
  tasks_datatable <- reactive({
    input$add_task_click
    load_data(table = "tasks")
  })
  
  # Show the studies in a table
  output$tasks_table <- DT::renderDataTable(
    DT::datatable(
      dplyr::arrange(tasks_datatable(), task_name) %>% 
        dplyr::select(-c(id, date_added)),
      rownames = FALSE, colnames = c("Task name", "Description"),
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
      rownames = FALSE, colnames = c("Title", "Main researcher", "Date started", "Date added"),
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
  
})
