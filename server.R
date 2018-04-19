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
shinyServer(function(input, output) {
  
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
    # Update the timestamp field to be the current time
    updateTextInput(session, "date_added", value = get_time_human())
    
    # Save the data
    save_data(form_data(), table = "studies")
    shinyjs::reset("add_study")
    updateTabsetPanel(session, "add_study", "view_studies")
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
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE, scrollX = TRUE)
    )
  )
  

  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  
})
