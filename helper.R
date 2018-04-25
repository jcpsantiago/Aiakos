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

appCSS <-
  "#.mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   .datatables { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: red; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "
