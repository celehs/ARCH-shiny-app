#' UI Module for header
#'
#' @description Module for header
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id Internal parameters for {shiny}.
#' @return sets of tags.
#' @examples
#' \dontrun{
#' headerUI('header')
#' }
#' @export
headerUI <- function(id, url_home){
  ns <- NS(id)
  tagList(
    actionButton(ns("home"), " Home",
                 icon = icon("home"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "Home page.", 
                 onclick =paste0("window.open('", url_home, "', '_blank')")
    ),
    downloadButton(ns("downloadData"),
                   " Download",
                   icon = icon("download"),
                   class = "btn btn-primary header-button",
                   width = "100px",
                   style = "padding: 6px;",
                   title = "The cosine similarity of current network."
    ),
    actionButton(ns("instruct"), " About",
                 icon = icon("book"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction of the app."
    ),
    actionButton(ns("help"), " Help",
                 icon = icon("question"),
                 class = "btn btn-primary header-button",
                 width = "100px",
                 style = "padding: 6px 20px 6px 20px;",
                 title = "The introduction tour."
    )
  )
}


#' Server Module for header
#'
#' @description Server Module for header
#'
#' @importFrom shiny NS tagList 
#' 
#' @param id string. Namespace of the module.
#' @param steps dataframe. The step-by-step introduction.
#' @param doc string. The path of the doc.
#' @param data dataframe. The data to download.
#' @return server part for header UI.
#' @examples
#' \dontrun{
#' headerServer('header')
#' }
#' @export
headerServer <- function(id, steps, doc, data) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    showNotification("Click 'Help' button to open step-by-step instructions.",
                     duration = 3, type = "warning")
    
    # observeEvent(input$help, {
    #   rintrojs::introjs(session,
    #                     options = list(
    #                       steps = steps,
    #                       showBullets = FALSE
    #                     )
    #   )
    # })
    
    ## downloading table================================================
    output$downloadData <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".xls", sep="")
        },
        content = function(path) {
          if(is.null(data)){
            data = data.frame("Warning"="Try to click some rows in the 'Possible inputs' box to specify your nodes!")
          }
          print(data)
          print(path)
          readr::write_tsv(data, path)
        }
      )
    
    observeEvent(input$instruct, {
        showModal(
          modalDialog(
            includeMarkdown(doc),
            title = "Instruction",
            size = "l"
          )
        )
      }
    )
    
    
  })
}




