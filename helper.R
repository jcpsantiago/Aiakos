# mandatory fields in the form
fields_mandatory_participants <- c(
  "first_name",
  "last_name"
)

fields_mandatory_study <- c(
  "title",
  "main_res",
  "task",
  "date_added"
)

get_time_human <- function() {
  format(Sys.time(), "%Y-%m-%d, %H:%M:%OS")
}
