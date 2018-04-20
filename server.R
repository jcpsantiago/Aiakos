#
# This is the Hopper Stopper server logic
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

library(shiny)
library(shinyjs)
library(magrittr)

source("helper.R")
source("storage.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  updateTextInput(session, "date_added", value = get_time_human())
  
  output$logo <- renderImage({
    list(src = here::here("figs/logo.svg"),
         height = "200px",
         contentType = "image/svg+xml")
  }, deleteFile = FALSE)
  
#### Add participant ####
  # Gather all the inputs and format tasks as a nice string
  part_data <- reactive({
    l <- sapply(fields_participant, function(x) x = input[[x]])
    l[["first_name"]] <- openssl::blake2b(tolower(l[["first_name"]]))
    l[["last_name"]] <- openssl::blake2b(tolower(l[["last_name"]]))
    names(l) <- c("first_name", "last_name", "date_of_birth", "study_id", "date_added")
    l
  })
  
  # When the Submit button is clicked 
  observeEvent(input$add_part_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(part_data(), "participants")
    
    reset("add_participant")
  })
  
  # Update the studies whenever a new submission is made
  # studies_datatable <- reactive({
  #   input$add_study_click
  #   load_data(table = "studies")
  # })

  
#### Add task ####
  #### Add participant ####
  # Gather all the inputs and format tasks as a nice string
  task_data <- reactive({
    l <- sapply(fields_task, function(x) x = input[[x]])
    names(l) <- c("name", "description")
    l
  })
  
  # When the Submit button is clicked 
  observeEvent(input$add_task_click, {
    
    # add date to date_added field
    # not yet implemented
    # updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(task_data(), "tasks")
    
    reset("add_task")
  })
  
  # Update the studies whenever a new submission is made
  tasks_datatable <- reactive({
    # input$add_study_click
    load_data(table = "tasks")
  })
  
  # Show the studies in a table
  output$tasks_table <- DT::renderDataTable(
    DT::datatable(
      dplyr::arrange(tasks_datatable(), name),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
  
    
#### Add study ####
  # Gather all the inputs and format tasks as a nice string
  studies_data <- reactive({
    sl <- sapply(fields_study, function(x) x = input[[x]])
    ## reorder alphabetically
    
    sl$tasks <- sapply(order(sl$tasks), function(x) x = sl$tasks[[x]])
    ## make a single single, instead of a vector of strings
    sl$tasks <- paste0(sl$tasks, collapse = ", ")
    sl
  })
  
  # When the Submit button is clicked 
  observeEvent(input$add_study_click, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(studies_data(), "studies")
    
    reset("add_study")
  })

  # Update the studies whenever a new submission is made
  studies_datatable <- reactive({
    input$add_study_click
    load_data(table = "studies")
  })
  
  # Show the studies in a table
  output$studies_table <- DT::renderDataTable(
    DT::datatable(
      studies_datatable(),
      rownames = FALSE, colnames = c("Title", "Main researcher", "Tasks", "Date started", "Date added"),
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
  
})
