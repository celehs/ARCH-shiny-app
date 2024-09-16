


#' mod_shapepicker
#'
#' @description module of shapepicker.
#' 
#' @param id string. type of the network node.
#' @param shape string. "square", "dot", "triangle", "star", "box", "circle",
#'                      "ellipse", "database", "text", "diamond".
#' @return shapepickerUI returns a pickerInput of shape for different types.
#'         shapepickerServer returns the value of shape pickerInput.
#' @examples
#' \dontrun{
#' shapepickerUI("NLP", "square")
#' shapepickerServer("NLP")
#' }
#' @export
shapepickerUI <- function(id, shape, shapes, icons) {
  ns <- NS(id)
  shinyWidgets::pickerInput(
    inputId = ns("shapepicker"),
    label = id, 
    choices = shapes,
    selected = shape,
    choicesOpt = list(
      icon = icons),
    options = list(
      `icon-base` = "fa")
  )
}

#' @rdname shapepickerUI
shapepickerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
   ns <- NS(id)
   input$shapepicker
})}




