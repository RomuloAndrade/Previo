box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    h1("Sobre o painel"),
    p("Apresenta os principais indicadores de vulnerabilidades 
      sociais dos bairros de Fortaleza, com o objetivo 
      de fortalecer as açoes estrategicas e definição dos territórios de atuação
      por meio de analise de dados descritiva."),
    h1("Autor"),
    p(
      "Rômulo Andrade",
      a(
        href = "mailto:romulo.andrade@iplanfor.fortaleza.ce.gov.br",
        "<romulo.andrade@iplanfor.fortaleza.ce.gov.br>"
      ),
      br(),
      tags$i("Núcleo de Sala Situacional - IPLANFOR")
    ),
    h1("Contribuições"),
      
    p(
      "Anderson Passos",
      a(
        href = "mailto:anderson.bezerra@iplanfor.fortaleza.ce.gov.br",
        "anderson.bezerra@iplanfor.fortaleza.ce.gov.brr"
      ),
      br(),
      tags$i("Observatório de Fortaleza - IPLANFOR")
    ),   
    p(
      "Elisângela Teixeira",
      a(
        href = "mailto:elisteixeira@letras.ufc.br",
        "elisteixeira@letras.ufc.br"
      ),
      br(),
      tags$i("Observatório de Fortaleza - IPLANFOR")
    ),
    p(
      "Felipe Neto",
      a(
        href = "mailto:felipefranklinneto@gmail.com",
        "felipefranklinneto@gmail.com"
      ),
      br(),
      tags$i("Observatório de Fortaleza - IPLANFOR")
    ),
    p(
      "Maria Gabrielle de Sousa",
      a(
        href = "mailto:gabrielle.santana@iplanfor.fortaleza.ce.gov.br",
        "gabrielle.santana@iplanfor.fortaleza.ce.gov.br"
      ),
      br(),
      tags$i("Observatório de Fortaleza - IPLANFOR")
    ),
    p(
      "Victor Santos",
      a(
        href = "mailto:victor.santos@iplanfor.fortaleza.ce.gov.br",
        "victor.santos@iplanfor.fortaleza.ce.gov.br"
      ),
      br(),
      tags$i("Observatório de Fortaleza - IPLANFOR")
    ),
    
    
    p(
      "Pedro Florêncio",
      a(
        href = "mailto:pedroflorenciocontato@gmail.com",
        "<pedroflorenciocontato@gmail.com>"
      ),
      br(),
      tags$i("Cientista de Dados - IPLANFOR")
    ),  
    
    h1("Código-Fonte"),
    p(
      a(
        href="https://gitlab.com/DIOBS/paineis/taxa-natalidade-bairros",
        "https://gitlab.com/DIOBS/paineis/taxa-natalidade-bairros"
      )
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {

  })
}
