
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


# 创建全局连接池变量
.db_pool <- NULL

# 初始化连接池
init_pool <- function(db) {
  if (is.null(.db_pool)) {
    .db_pool <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = db$dbname,
      host = db$host,
      port = db$port,
      user = db$user,
      password = db$password,
      minSize = db$minSize,
      maxSize = db$maxSize,
      idleTimeout = db$idleTimeout 
    )
    
    # 注册应用关闭时关闭连接池
    shiny::onStop(function() {
      if (!is.null(.db_pool)) {
        pool::poolClose(.db_pool)
        .db_pool <<- NULL
      }
    })
  }
  return(.db_pool)
}

# 获取连接池连接
get_pool <- function(db) {
  if (is.null(.db_pool)) {
    init_pool(db)
  }
  return(.db_pool)
}

readDB <- function(sql, tname, db){
  pool <- get_pool(db)
  
  # 检查表是否存在
  table_exists <- pool::dbExistsTable(pool, tname)
  
  if(table_exists){
    tryCatch({
      warning("Start to connect to db")
      pool::dbGetQuery(pool, sql)
    }, error = function(e) {
      warning("Query failed: ", e$message)
      NULL
    })
  } else {
    warning(paste0("Table '", tname, "' doesn't exist in the database!"))
    NULL
  }
}


con <- function(db){
  tryCatch({
    DBI::dbConnect(
      drv = RPostgres::Postgres(),
      dbname = db$dbname,
      host = db$host,
      port = db$port,
      user = db$user,
      password = db$password
    )
  }, error = function(e) {
    stop("Failed to connect to database: ", e$message)
  })
}

