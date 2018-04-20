# mandatory fields in the form
fields_participant <- c(
  "first_name",
  "last_name",
  "date_of_birth",
  "study_name",
  "date_added"
)

fields_study <- c(
  "title",
  "main_res",
  "tasks",
  "date_started",
  "date_added"
)

fields_task <- c(
  "task_name",
  "task_desc"
)

get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS")
}