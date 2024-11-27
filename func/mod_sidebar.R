#' Module for sidebar
#'
#' @description Module for sidebar
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id string. namespace the module.
#' @return sets of tags.
#' @examples
#' \dontrun{
#' sidebarUI('input')
#' }
#' @export
sidebarUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("ui_input")),
    br(),
    div(id = "buttons",
    fluidRow(
      column(6,
             div(
               actionButton(
                 inputId = ns("deselect"),
                 label = "Unselect", 
                 icon = icon("undo"),
                 color = "lightgrey"
               ), align = "center")),
      column(6,
             div(
               actionButton(
                 inputId = ns("gobutton"),
                 label = "Submit", 
                 icon = icon("check"),
                 color = "green"
               ), align = "center")))
    )
  )
}

#' Server Module for sidebar
#'
#' @rdname sidebarUI
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id string. Namespace of the module.
#' @param tb_input string. Namespace of the module.
#' @param tname string. table name in db to search.
#' @param db string. name of the database.
#' @param type string. Default DT. DT or reactable.
#' @param selected numeric. Pre-selected rows in table. Default c(1,3).
#' @param init_nodes vector. Default NULL. Initial center nodes.
#' @param server logical. Default TRUE, the data is kept on the server and
#'     the browser requests a page at a time; if FALSE, then the entire 
#'     data frame is sent to the browser at once. Highly recommended for 
#'     medium to large data frames, which can cause browsers to slow down or 
#'     crash. Passed to renderDT().
#' @return vector of center nodes.
#' @examples
#' \dontrun{
#' sidebarServer('input')
#' }
#' @export
sidebarServer <- function(id, tb_input, tname, db, type = 1, selected = c(1, 3),
                          init_nodes = NULL,
                          server = TRUE) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    output$ui_input <- renderUI({
      if(type == 1){
        tagList(shinycssloaders::withSpinner(
            DT::DTOutput(ns("tbInput")), type = 6))
      } else {
        tagList(
          shinyWidgets::searchInput(
            inputId = ns("searchbox2"),
            label = "Enter your search: ",
            placeholder = "rheumatoid arthritis",
            value = NULL,
            btnSearch = icon("search"),
            width = "100%"
          ),
          shinycssloaders::withSpinner(
            DT::DTOutput(ns("tbInput")), type = 6)
        )
      }
    })
  
    df_search <- function(text, tname, db){
      if("synonyms" %in% RPostgres::dbListTables(con(db))){
        sql <- paste0('SELECT s.id, "term", "synonyms"
        FROM (SELECT "id", "term" FROM "', tname, '") AS s
        LEFT JOIN "synonyms" AS r
        ON (s.id = r.id) WHERE s.id ilike \'%', text, '%\' or "term" ilike \'%', text, '%\' or "synonyms" ilike \'%', text, '%\';')
      } else {
        sql <- paste0('SELECT "id", "term" FROM "', tname, '"
          WHERE "id" ilike \'%', text, '%\' or "term" ilike \'%', text, '%\';')
      }
      df <- readDB(sql, tname, db)
      df <- df[!duplicated(df$id), ]
      
      df$order <- 1
      df$order[tolower(df$term) == text] <- 0
      if("synonyms" %in% colnames(df)){
        df$order[tolower(df$synonyms) == text] <- 0
      }
      df[order(df$order, df$id), 1:(ncol(df)-1)]
    }
    
    tb_input2 <- reactive({
      if(type == 1){
        tb_input
      } else {
        if(isTruthy(input$searchbox2)){
          text <- input$searchbox2
        } else {
          text <- "rheumatoid arthritis"
        }
        # ids <- search(text)
        # tb_input[tb_input$id %in% ids,]
        df_search(text, tname, db)
      }
    })
    
    rows <- reactive({
      if(type == 1 | (!isTruthy(input$searchbox2))){
        selected
      } else {
        NULL
      }
    })
    
    output$tbInput <- DT::renderDT(DT::datatable({
      print("-------------------")
      tb_input2()
    }, rownames = FALSE,
    options = list(
      paging = FALSE,
      scrollY = "300px",
      scrollCollapse = TRUE,
      dom = ifelse(type == 1, "Bfrtp", "Brtp")
    ),
    selection = list(mode = 'single', 
                     selected = rows(), 
                     target = 'row'),
    escape = FALSE
    ), server = server)


  ## Update checkboxinput if refreshing=================================
  observeEvent(input$deselect, {

    DT::reloadData(
      DT::dataTableProxy('tbInput'),
      resetPaging = TRUE,
      clearSelection = c("all"))

  })
  
  ## center nodes ==============================
  center_nodes <- eventReactive(input$gobutton, {
    if(input$gobutton == 0 & str_detect(session$clientData$url_search, 'centernode=')){
      url_vars <- session$clientData$url_search
      str_split(url_vars, 'centernode=')[[1]][2]
    }else{
      if(input$gobutton == 0 & !isTruthy(input$tbInput_rows_selected)){
        init_nodes
      } else {
        tb_input2()$id[input$tbInput_rows_selected]
      } 
    }
  }, ignoreNULL = FALSE)
  
  reactive({
    center_nodes()
  })
})}

