
#' Get/write data from a PostgreSQL database
#' 
#' @description
#' getData: get the entire table or some rows from the table from a PostgreSQL database.
#' data2db: write the table into a PostgreSQL database.
#' 
#' @param id string. The id of the data to get from the table. When NULL, the entire table will be returned.
#' @param tname string. The table name of the data.  A table name can contain numeric alphabets and underline (e.g. table_1 ). The table name  should begin with an alphabet. 
#'              The 3 table names below are specified: "df_edges": data for network; "dict": dictionary of the nodes; "details".
#' @param db string. The name of the database.
#' @param field string. Default "from". The field name of the id to get from the table.
#' @param data data.frame. The data to write into the database.
#' @param title string. Default NULL. Parameter for more data. The title for more information data.
#' @param note string. Default "". Parameter for more data. The description for more information data.
#' @return NULL
#' @examples
#' \dontrun{
#' getData("PheCode:250", "df_edges", "app_cuinetwork")
#' data2db(df_edges, "df_edges", "app_cuinetwork")
#' }
#' @export
getData <- function(id, tname, db, field = "from"){
  if(is.null(id)){
    sql <- paste0('SELECT * FROM "', tname, '";')
  } else {
    sql <- paste0('SELECT * FROM "', tname, '" WHERE "', field, '" = \'', id, '\';')
  }
  readDB(sql, tname, db)
}


#' @rdname getData
#' @export
data2db <- function(data, tname, db, title = NA, note = NA){
  if(!is.data.frame(data)) {
    stop("Input 'data' must be a data.frame")
  }
  
  if(is.null(tname) || !is.character(tname)) {
    stop("Table name must be a non-null character string")
  }
  
  cat("Database: ", db$dbname, "\n")
  cat("Table: ", tname, "\n")
  if(!tname %in% c("df_edges", "dict")) {
    conn <- con(db)
    on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
    
    if(!"details" %in% RPostgres::dbListTables(conn)){
      details <- data.frame(tname = tname, title = title, note = note)
    } else {
      details <- getData(NULL, "details", db)
      if(tname %in% details$tname){
        details$title[details$tname == tname] <- title
        details$note[details$tname == tname] <- note
      } else {
        details <- rbind(details, data.frame(tname = tname, title = title, note = note))
      }
    }
    writeTable(details, "details", db)
  }
  writeTable(data, tname, db)
}


writeTable <- function(data, tname, db){
  if(nrow(data) == 0) {
    warning("Writing empty data frame to database")
  }
  
  conn <- con(db)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  message("Writing to database...")
  tryCatch({
    RPostgres::dbWriteTable(conn, tname, data, overwrite = TRUE)
    message("Data successfully written to table: ", tname)
  }, error = function(e) {
    stop("Failed to write to database: ", e$message)
  })
}

con <- function(db){
  tryCatch({
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = db$dbname,
      # host = Sys.getenv("DB_HOST"),
      host = db$host,
      # port = as.numeric(Sys.getenv("DB_PORT")),
      port = db$port,
      # user = Sys.getenv("DB_USER"),
      user = db$user,
      # password = Sys.getenv("DB_PSWD")
      password = db$password
      # password = rstudioapi::askForPassword("Database password")
    )
  }, error = function(e) {
    stop("Failed to connect to database: ", e$message)
  })
}

readDB <- function(sql, tname, db){
  conn <- con(db)
  on.exit(RPostgres::dbDisconnect(conn), add = TRUE)
  
  if(tname %in% RPostgres::dbListTables(conn)){
    tryCatch({
      RPostgres::dbGetQuery(conn = conn, sql)
    }, error = function(e) {
      warning("Query failed: ", e$message)
      NULL
    })
  } else {
    warning(paste0("Table '", tname, "' doesn't exist in the database!"))
    NULL
  }
}

