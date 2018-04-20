#
# This is the Hopper Stopper server logic
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

library(shiny)
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
  
  # Gather all the form inputs
  form_data <- reactive({
    sapply(fields_mandatory_study, function(x) x = input[[x]])
  })
  
  # When the Submit button is clicked 
  observeEvent(input$submit_study, {
    
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data(form_data(), "studies")
    
    reset("add_study")
  })
  
  
  # Update the responses whenever a new submission is made
  responses_data <- reactive({
    input$submit_study
    load_data(table = "studies")
  })
  
  # Show the responses in a table
  output$studies_table <- DT::renderDataTable(
    DT::datatable(
      responses_data(),
      rownames = FALSE, colnames = c("Title", "Main researcher", "Tasks", "Date added"),
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
})
