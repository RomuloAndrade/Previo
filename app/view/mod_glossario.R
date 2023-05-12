#' glossario UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom bs4Dash accordion
mod_glossario_ui <- function(id){
  ns <- NS(id)

  tagList(
    fluidRow(
      class = "align-center justify-content-center text-center mt-2",
      column(
        width = 8,
        h1("Glossário"),
        hr(class = "divider")
      )
    ),
    fluidRow(
      constroi_glossario(nchunks = 3)
    )

  )
}

#' glossario Server Functions
#'
#' @noRd 
mod_glossario_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}

## To be copied in the UI
# mod_glossario_ui("glossario_1")

## To be copied in the server
# mod_glossario_server("glossario_1")
