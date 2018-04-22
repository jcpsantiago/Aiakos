# mandatory fields in the form
fields_participant <- c(
  "first_name" = "first_name",
  "last_name" = "last_name",
  "date_of_birth" = "date_of_birth",
  "date_added" = "date_added"
)

fields_study <- c(
  "study_title",
  "contact_person",
  "year_started",
  "date_added"
)

fields_task <- c(
  "task_name",
  "task_desc"
)

get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
}