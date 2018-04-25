library(shiny)
library(shinyjs)
library(shinythemes)
library(pool)
library(RSQLite)
library(dplyr)
library(purrr)

source("helper.R")
source("storage.R")

## open the connction to the database
pool <- dbPool(RSQLite::SQLite(), dbname = "hopper.db")

## when the app stops, close all open connections
onStop(function() {
  poolClose(pool)
})



#### Define UI for application ####
ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    theme = shinytheme("cosmo"),
    inlineCSS(appCSS),
    br(),
    
    #### Application title ####
    titlePanel(div(
      paste("Study Butler", emo::ji("groom")),
      h4("Institute of Medical Psychology")
    ),
    windowTitle = "Butler"),
    
    #### Sidebar ####
    sidebarLayout(
      sidebarPanel(width = 2,
                   div(style = "text-align:center",
                       h1(emo::ji(
                         "brain"
                       )),
                       p(
                         a(icon("github", "fa-2x"), href = "https://github.com/jcpsantiago/hopperStopper"),
                         a(icon("envelope", "fa-2x"), href = "mailto:joao.santiago@uni-tuebingen.de?Subject=Help!%20Study%20Butler%20not%20working!", target =
                             "_top")
                       ),
                       br(), br())),
      
      # main panel with tabs
      mainPanel(
        width = 10,
        tabsetPanel(
          #### participants form ####
          tabPanel(
            icon = icon("plus"),
            "Add participant",
            width = 5,
            div(
              id = "add_participant",
              textInput(
                inputId = "first_name",
                label = "First name",
                value = "Joao"
              ),
              textInput(
                inputId = "last_name",
                label = "Last name",
                value = ""
              ),
              hidden(div(
                id = "part_test_info",
                p(
                  strong("WARNING:"),
                  "This person has taken part in
                  other studies!",
                  emo::ji("hand")
                ),
                p("Make sure there are no conflicts."),
                tableOutput(outputId = "part_test")
              )),
              dateInput(
                inputId = "date_of_birth",
                label = "Date of birth",
                value = "1990-01-29",
                format = "yyyy-mm-dd",
                startview = "year",
                language = "en"
              )
              ),
            selectInput("study_title_sel", "Select study",
                        levels(
                          as.factor(pool %>%
                                      tbl("studies") %>%
                                      select(study_title) %>%
                                      pull())
                        )),
            hidden(textInput("date_added", "")),
            actionButton(
              inputId = "add_part_click",
              label = "Submit",
              icon = icon("check")
            )
          ),
          
          #### studies form ####
          tabPanel(
            icon = icon("plus"),
            "Add study",
            div(
              id = "add_study",
              textInput(
                inputId = "study_title",
                label = "Title of the study",
                value = ""
              ),
              div(
                id = "study_test_info",
                p(strong("I'm sorry, but this name is taken!")),
                p(
                  "If your study is listed on the table below,",
                  br(),
                  "just select it when adding a new participant.",
                  emo::ji("wink")
                ),
                tableOutput(outputId = "study_test_table"),
                br()
              ),
              textInput(inputId = "contact_person",
                        label = "Name of the contact person"),
              selectInput(
                inputId = "year_started",
                label = "Year started",
                choices = seq(from = 2014, to = 2100, by = 1),
                selected = 2018
              ),
              selectInput(
                inputId = "tasks",
                label = "Select tasks",
                choices = levels(as.factor(
                  pool %>%
                    tbl("tasks") %>%
                    select(task_name) %>%
                    pull()
                )),
                selected = "",
                selectize = TRUE,
                multiple = TRUE
              )
            ),
            hidden(textInput("date_added", "")),
            actionButton(
              inputId = "add_study_click",
              label = "Submit",
              icon = icon("check")
            )
          ),
          
          #### tasks form ####
          tabPanel(
            icon = icon("plus"),
            "Add task",
            div(
              id = "add_task",
              shinyTypeahead::typeaheadInput(
                inputId = "task_name",
                label = "Task name",
                value = "",
                choices = levels(as.factor(
                  pool %>%
                    tbl("tasks") %>%
                    select(task_name) %>%
                    pull()
                )),
                items = 3,
                minLength = 1
              ),
              div(
                id = "task_check_error",
                p(
                  strong("This task is already in the database."),
                  # utf8::utf8_print(intToUtf8(0x1f605)) use this is emo::ji()
                  # doesn't work for some reason
                  emo::ji("sweat_smile")
                ),
                p("See below if it's the same, otherwise choose another name."),
                tableOutput(outputId = "task_test_table"),
                br()
              ),
              textInput(
                inputId = "task_desc",
                label = "Short description",
                value = ""
              )
            ),
            actionButton(
              inputId = "add_task_click",
              label = "Submit",
              icon = icon("check")
            )
          ),
          
          #### view tasks ####
          tabPanel(
            icon = icon("clipboard"),
            "Tasks",
            DT::dataTableOutput("tasks_table")
          ),
          
          #### view studies ####
          tabPanel(
            id = "view_studies",
            icon = icon("flask"),
            "Studies",
            DT::dataTableOutput("studies_table")
          )
        )
          )
  )
  )
)



# Define server logic
#
# This is the Hopper Stopper server logic
# written by Joao Santiago <joao.santiago@uni-tuebingen.de>
# contact me in case of trouble with this code :)
#

#### Define server logic required to draw a histogram ####
server <- shinyServer(function(input, output, session) {
  
  ## set initial value for the hidden date_added parameter
  updateTextInput(session, "date_added", value = get_time_human())

  
  ## placeholder code for an image, in case we want to add a logo in the future
  # output$logo <- renderImage({
  #   list(src = here::here("figs/logo.svg"),
  #        height = "200px",
  #        contentType = "image/svg+xml")
  # }, deleteFile = FALSE)
  
  #### Add participant ####

  ## whenever the user changes the first and last name field, filter the
  ## database 
  df_part <- eventReactive({
    input$first_name
    input$last_name
  }, {
    disable("add_part_click")
    
    pool %>% tbl("participants") %>%
      filter(
        first_name == stringr::str_extract(openssl::sha256(tolower(input$first_name)), "[0-9a-z]+") &
          last_name == stringr::str_extract(openssl::sha256(tolower(input$last_name)), "[0-9a-z]+")
      ) %>%
      collect
    
  })
  
  ## whenever df_part changes, create a table resulting from the join of studies
  ## and tasks matching the names given by the user
  df_part_joined <- eventReactive(df_part(), {
    df_part() %>%
      left_join(
        .,
        pool %>% tbl("part_study"),
        by = c("id" = "part_reference"),
        copy = TRUE
      ) %>%
      left_join(., pool %>% tbl("study_task"), by = "study_reference", copy = TRUE) %>%
      left_join(
        .,
        pool %>% tbl("studies"),
        by = c("study_reference" = "id"),
        copy = TRUE
      ) %>%
      left_join(.,
                pool %>% tbl("tasks"),
                by = c("task_reference" = "id"),
                copy = TRUE) %>%
      select(c(study_title, year_started, task_name)) %>%
      arrange(task_name) %>%
      distinct() %>%
      group_by(study_title, year_started) %>%
      summarize(tasks = stringr::str_flatten(task_name, collapse = ", ")) %>%
      rename(
        "Study title" = study_title,
        "Year study started" = year_started,
        "Tasks" = tasks
      )
    
  })
  
  ## show a warning and a table containing the studies matching the first and
  ## last name given by the user
  observeEvent(df_part_joined(), {
    toggleElement("part_test_info", condition = nrow(df_part_joined()) > 0)
    output$part_test <- renderTable(df_part_joined())
  })
  
  ## activate the submit button when both first and last names do not match 
  ## the database and the fields are not empty
  observeEvent(df_part(), {
    toggleState(
      "add_part_click",
      condition = nchar(input$first_name) > 0 && nchar(input$last_name) > 0
    )
  })
  
  ## gather all inputs present in the fields_participants (see helper.R file)
  part_data <- reactive({
    map_df(fields_participant, ~ input[[.]])
  })
  
  # When the Submit button is clicked
  observeEvent(input$add_part_click, {
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data_tidy(pool, part_data(), "participants")
    # save the participant/study relationship into the part_study table
    map_part_study(input$study_title_sel)
    
    # delete all user input from the add participant tab
    reset("add_participant")
  })
  
  
  #### Add task ####
  
  ## check if the entered task is already in the database, returns FALSE
  ## as long as there is no match
  df_task <- eventReactive(input$task_name, {
    disable("add_task_click")
    
    pulled_tasks <- pool %>% tbl("tasks") %>% select(task_name) %>%
      pull() %>% map_chr(., ~ lower_squish_str(.))
    
    return(lower_squish_str(input$task_name) %in% pulled_tasks)
  })
  
  ## whenever df_task() changes, retrieve data from the database matching the
  ## entered task
  df_task_joined <- eventReactive(df_task(), {
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
  
  ## whenever df_task_joined changes, show the warning message and the table
  ## with description, if df_task_joined has more than one row i.e. there's a
  ## match between the chosen name and the database
  observeEvent(df_task_joined(), {
    toggleElement("task_check_error",
                  condition = nrow(df_task_joined()) > 0)
    
    output$task_test_table <- renderTable({
      df_task_joined()
    })
    # print(nrow(df_task_joined()) > 0) ONLY FOR DEBUGGING !
    
  })
  
  ## whenever if there task_name and task_desc are not empty, or there is a match
  ## between the entered name and the database, keep the submit button disabled
  observeEvent({
    df_task()
    input$task_desc
  }, {
    toggleState(
      "add_task_click",
      condition = !(df_task()) &&
        nchar(input$task_name) > 0 && nchar(input$task_desc) > 0
    )
  })
  
  
  # Gather all the inputs and format tasks as a nice string
  task_data <- reactive({
    map_df(fields_task, ~ input[[.]]) %>%
      rename("description" = "task_desc")
    
    # l <- sapply(fields_task, function(x)
    #   x = input[[x]])
    # names(l) <- c("task_name", "description")
    # l
  })
  
  
  # When the Submit button is clicked
  observeEvent(input$add_task_click, {
    # add date to (hidden) date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data_tidy(pool, task_data(), "tasks")
    
    # delete all user inputs in the tasks tab
    reset("add_task")
    
    ## update the task selection in the add study tab
    ## to include the newly added study
    updateSelectInput(session, "tasks",
                      choices = levels(as.factor(
                        pool %>%
                          tbl("tasks") %>%
                          select(task_name) %>%
                          pull()
                      )))
    
    ## update the typeahead box in the tasks tab
    shinyTypeahead::updateTypeaheadInput(session, "task_name",
                                         choices = levels(as.factor(
                                           pool %>%
                                             tbl("tasks") %>%
                                             select(task_name) %>%
                                             pull()
                                         )))
  })
  
  ## Update the studies tab/table whenever a new submission is made
  tasks_datatable <- reactive({
    input$add_task_click
    
    pool %>% tbl("tasks") %>%
      select(-c(id, date_added)) %>%
      arrange(task_name) %>%
      collect
  })
  
  ## Show the studies currently in the database in a table
  output$tasks_table <- DT::renderDataTable(
    DT::datatable(
      tasks_datatable(),
      rownames = FALSE,
      colnames = c("Task name", "Description"),
      style = "bootstrap",
      options = list(
        searching = TRUE,
        lengthChange = TRUE,
        scroller = TRUE
      )
    )
  )
  
  
  #### Add study ####

  ## whenever study_title changes verify if it is already in the database
  df_study <- eventReactive(input$study_title, {
    disable("add_study_click")
    
    pulled_studies <-
      pool %>% tbl("studies") %>% select(study_title) %>%
      pull() %>% map_chr(., ~ lower_squish_str(.))
    
    return(lower_squish_str(input$study_title) %in% pulled_studies)
  })
  
  ## whenever df_study() changes, retrieve data from the database matching the
  ## entered task
  df_study_joined <- eventReactive(df_study(), {
    lower_case_study_title <- lower_squish_str(input$study_title)
    print(lower_case_study_title)
    
    pool %>% tbl("studies") %>%
      left_join(., pool %>% tbl("study_task"), by = c("id" = "study_reference")) %>%
      left_join(., pool %>% tbl("tasks"), by = c("task_reference" = "id")) %>%
      select(study_title, contact_person, year_started, task_name) %>%
      arrange(task_name) %>%
      distinct() %>%
      collect %>%
      group_by(study_title, contact_person, year_started) %>%
      summarize(tasks = stringr::str_flatten(task_name, collapse = ", ")) %>%
      arrange(desc(year_started)) %>%
      mutate(study_title_squished = lower_squish_str(study_title)) %>%
      filter(study_title_squished == lower_case_study_title) %>%
      select(-study_title_squished) %>%
      rename(
        "Study title" = study_title,
        "Contact person" = contact_person,
        "Year study started" = year_started,
        "Tasks" = tasks
      )
  })
  
  ## whenever df_study_joined changes, show the warning message and the table
  ## with description, if df_study_joined has more than one row i.e. there's a
  ## match between the chosen name and the database
  observeEvent(df_study_joined(), {
    print(nrow(df_study_joined()))
    toggleElement("study_test_info",
                  condition = nrow(df_study_joined()) > 0)
    
    output$study_test_table <- renderTable({
      df_study_joined()
    })
    
  })
  
  ## whenever if there task_name and task_desc are not empty, or there is a match
  ## between the entered name and the database, keep the submit button disabled
  observe({
    toggleState(
      "add_study_click",
      condition = !(df_study()) &&
        nchar(input$study_title) > 0 &&
        nchar(input$contact_person) > 0 && length(input$tasks) > 0
    )
    
  })
  
  
  # Gather all the inputs
  studies_data <- reactive({
    map_df(fields_study, ~ input[[.]])
    # sapply(fields_study, function(x) x = input[[x]])
  })
  
  # When the Submit button is clicked
  observeEvent(input$add_study_click, {
    # add date to date_added field
    updateTextInput(session, "date_added", value = get_time_human())
    
    # save data to database
    save_data_tidy(pool, studies_data(), "studies")
    
    # map study to tasks
    purrr::walk(input$tasks, ~ map_study_task(input$study_title, .))
    
    # delete all user inputs in the study tab
    reset("add_study")
    
    updateSelectInput(session, "study_title_sel",
                      choices = levels(as.factor(
                        pool %>%
                          tbl("studies") %>%
                          select(study_title) %>%
                          pull()
                      )))
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
      rownames = FALSE,
      style = "bootstrap",
      colnames = c("Title", "Main researcher", "Date started", "Date added"),
      options = list(
        searching = TRUE,
        lengthChange = TRUE,
        scrollX = TRUE
      )
    )
  )
  
})


shinyApp(ui, server)
