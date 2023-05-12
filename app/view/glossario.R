box::use(
  shiny[...],
  bs4Dash[...],
  readxl[read_excel]
)

## funcao glossario
glossario<- readxl::read_excel('app/data/Glossário.xlsx',col_names=T,.name_repair = "unique") 
constroi_glossario <- function(nchunks = 3) {
  df <- split(
    glossario,
    rep(1:nchunks,
        length.out = nrow(glossario),
        each = ceiling(nrow(glossario)/nchunks)))
  
  width <- 12%/%nchunks
  
  lapply(1:nchunks, function(i){
    div(
      class = sprintf("col-xl-%s", width),
      bs4Dash::box(
        width = 12,
        closable = FALSE,
        maximizable = TRUE,
        collapsible = FALSE,
        do.call("accordion", c(
          list(id = sprintf('accordion_glossario_%s', i)),
          apply(df[[i]], 1, function(r) bs4Dash::accordionItem(title = r[1], r[2]))))))})
}

#' @export
ui <- function(id) {
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

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}
