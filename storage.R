sqlitePath <- "hopper.db"

save_data_tidy <- function(con, df, table){

  if(!(is.null(df$date_of_birth))){
    df %>%
      mutate(date_of_birth = stringr::str_extract(openssl::sha256(as.character(date_of_birth)), "[0-9a-z]+"),
             first_name = openssl::sha256(tolower(first_name)),
             last_name = openssl::sha256(tolower(last_name))) %>%
      db_insert_into(con, table, .)
  } else
    db_insert_into(con, table, df)
}

map_study_task <- function(study, task){
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)

    # Construct the update query
  query <- sprintf(
    "INSERT INTO study_task VALUES ((SELECT id FROM studies WHERE study_title = '%s'),
                                    (SELECT id FROM tasks WHERE task_name = '%s'))",
    study,
    task)
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

map_part_study <- function(study){
  # Connect to the database
  db <- dbConnect(SQLite(), sqlitePath)
  
  # Construct the update query
  query <- sprintf(
    "INSERT INTO part_study VALUES ((SELECT MAX(id) FROM participants),
                                    (SELECT id FROM studies WHERE study_title = '%s'));",
    study)
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}

# save_data <- function(data, table) {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the update query by looping over the data fields
#   query <- sprintf(
#     "INSERT INTO %s (%s) VALUES ('%s')",
#     table, 
#     paste(names(data), collapse = ", "),
#     paste(data, collapse = "', '")
#   )
#   # Submit the update query and disconnect
#   dbGetQuery(db, query)
#   dbDisconnect(db)
# }

# load_data <- function(table) {
#   # Connect to the database
#   db <- dbConnect(SQLite(), sqlitePath)
#   # Construct the fetching query
#   query <- sprintf("SELECT * FROM %s", table)
#   # Submit the fetch query and disconnect
#   data <- dbGetQuery(db, query)
#   dbDisconnect(db)
#   data
# }
