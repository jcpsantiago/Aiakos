# mandatory fields in the form
fields_participant <- c(
  "first_name" = "first_name",
  "last_name" = "last_name",
  "date_of_birth" = "date_of_birth",
  "date_added" = "date_added"
)

fields_study <- c(
  "study_title" = "study_title",
  "contact_person" = "contact_person",
  "year_started" = "year_started",
  "date_added" = "date_added"
)

fields_task <- c(
  "task_name" = "task_name",
  "task_desc" = "task_desc"
)

quick_stat <- function(table, .pool){
  .pool %>% tbl(table) %>% distinct() %>% collect %>% nrow()
}

get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
}

lower_squish_str <- function(string){
  tolower(paste(unlist(strsplit(string," ")),collapse=""))
}

# labelMandatory <- function(label) {
#   tagList(
#     label,
#     span("*", class = "mandatory_star")
#   )
# }

# appCSS <-
#   "#.mandatory_star { color: red; }
#    #submit_msg { margin-left: 15px; }
#    #error { color: red; }
#    body { background: #fcfcfc; }
#    #header { background: red; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
#   "

side_stats <- function(.pool){
  h4(
    strong(quick_stat("studies", .pool)),
    "studies",
    br(),
    strong(quick_stat("tasks", .pool)),
    "tasks",
    br(),
    strong(quick_stat("participants", .pool)),
    "unique participants"
  )
}

# All the code in this file needs to be copied to your Shiny app, and you need
# to call `withBusyIndicatorUI()` and `withBusyIndicatorServer()` in your app.
# You can also include the `appCSS` in your UI, as the example app shows.

# =============================================

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

# Call this function from the server with the button id that is clicked and the
# expression to run when the button is clicked
withBusyIndicatorServer <- function(buttonId, expr) {

  doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  
  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch({
    value <- expr
    shinyjs::show(selector = doneEl)
    shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                       time = 0.5))
    value
  }, error = function(err) { errorFunc(err, buttonId) })
}

# When an error happens after a button click, show the error
errorFunc <- function(err, buttonId) {
  errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
  errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
  errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

# Create a little question mark link that shows a help popup on hover
helpPopup <- function(content, title = NULL) {
  a(href = "#",
    class = "popover-link",
    `data-toggle` = "popover",
    `data-title` = title,
    `data-content` = content,
    `data-html` = "true",
    `data-trigger` = "hover",
    icon("question-circle")
  )
}

appCSS <- "
.tabbable > .nav > li[class=active] > a {
           color: #158CBA;
}
.nav-tabs a:focus, 
.nav-tabs a:hover {
    color: #657DB1;
}
.btn-done-indicator {
  color: #28b62c;
}
.shiny-input-container { margin-top: 15px; }
.datatables { margin-top: 15px; }
a {
  color: #158CBA;
  text-decoration: none;
  background-color: transparent;
  -webkit-text-decoration-skip: objects;
}
a:hover {
  color: #0d5875;
  text-decoration: underline;
}
"
