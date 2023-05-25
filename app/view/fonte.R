box::use(
  shiny[...],
  bs4Dash[...]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(
  fluidRow(
    class = "align-center justify-content-center text-center mt-2",
    column(
      width = 8,
      h1("Fonte de dados"),
      hr(class = "divider")
    )
  ),
  fluidRow(
    
    column(width = 4,
    br(),
    h3("Suporte a segurança pública"),
    br(),
    p(strong("Mediação de conflitos"),
      a(target="_blank",
        href = "https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
        "<link>"
      ),
      br(),
      tags$i("SESEC/COPSEC, 2023.") ),
    
    ##
    p(strong("Videomonitoramento"),
      # a(target="_blank",
      #   href = "mailto:https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
      #   "<link>"
      # ),
      br(),
      tags$i("AMC/SESEC, 2023.") ),
    ##
    p(strong("Iluminação pública"),
      # a(target="_blank",
      #   href = "mailto:https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
      #   "<link>"
      # ),
      br(),
      tags$i("SCSP, 2023.") ),
    ##
    p(strong("Territórios do Plano Municipal de Proteção Urbana - PMPU"),
      # a(target="_blank",
      #   href = "mailto:https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
      #   "<link>"
      # ),
      br(),
      tags$i("SESEC/COPSEC, 2023.") ),
    ##
    p(strong("Territórios do Grupamento Patrulha Maria da Penha - GEMP"),
      # a(target="_blank",
      #   href = "mailto:https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
      #   "<link>"
      # ),
      br(),
      tags$i("SESEC/COPSEC, 2023.") ),
    ##
    ##
    p(strong("Células de proteção cumunitária"),
      # a(target="_blank",
      #   href = "mailto:https://seguranca.fortaleza.ce.gov.br/sesec/mediacaodeconflitos.html",
      #   "<link>"
      # ),
      br(),
      tags$i("SESEC/COPSEC, 2023.") ),
    ##
      ),
    column(width = 4,
    br(),
    h3("Socioeconômicos"),
    br(),
    p(strong("Aglomerados Subnormais"),
      a(target="_blank",
        href = "https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15788-aglomerados-subnormais.html",
        "<link>"
      ),
      br(),
      tags$i("IBGE, 2019.") ),
    ##
    p(strong("Ecopontos"),
      a(target="_blank",
        href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/349",
        "<link>"
      ),
      br(),
      tags$i("SCSP, 2022") ),
    ##
    p(strong("Vínculos formais"),
      a(target="_blank",
        href = "http://pdet.mte.gov.br/microdados-rais-e-caged",
        "<link>"
      ),
      br(),
      tags$i("RAIS, 2021") ),
    ##
    p(strong("Vazios urbanos"),
      a(target="_blank",
        href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/655",
        "<link>"
      ),
      br(),
      tags$i("IPLANFOR/SEINF/SEUMA, 2018.") ),
    ##
    p(strong("Gravidez na adolescência"),
      a(target="_blank",
        href = "https://svs.aids.gov.br/daent/cgiae/sinasc/",
        "<link>"
      ),
      br(),
      tags$i("SINASC/DataSUS, 2022.") ),
   
      ),
    column(width = 4,
           br(),
           h3("Infraestrutura"),
           br(),
           p(strong("Adoção de Áreas Verdes"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/622",
               "<link>"
             ),
             br(),
             tags$i("SEUMA, 2023.") ),
           ##      
           p(strong("Areninhas"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/348",
               "<link>"
             ),
             br(),
             tags$i("SCSP, 2022.") ),
           ## 
           p(strong("Assistência Social"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/48",
               "<link>"
             ),
             br(),
             tags$i("Mais Ação, 2020.") ),
           ## 
           p(strong("Base Samu"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/321",
               "<link>"
             ),
             br(),
             tags$i("SMS, 2020.") ),
           ## 
           p(strong("Conselhos Tutelares"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/49",
               "<link>"
             ),
             br(),
             tags$i("MPCE, 2020.") ),
           ## 
           p(strong("Equipamentos de cultura"),
             # a(target="_blank",
             #   href = "https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15788-aglomerados-subnormais.html",
             #   "<link>"
             # ),
             br(),
             tags$i("F2040, 2019.") ),
           ## 
           p(strong("Equipamentos de Saúde"),
             a(target="_blank",
               href = "https://cnes.datasus.gov.br/",
               "<link>"
             ),
             br(),
             tags$i("CNES/DataSUS, 2023.") ),
           ## 
           p(strong("Escolas municipais"),
             # a(target="_blank",
             #   href = "https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15788-aglomerados-subnormais.html",
             #   "<link>"
             # ),
              br(),
             tags$i("SME, 2023") ),
           ## 
           p(strong("Praças"),
             a(target="_blank",
               href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/99",
               "<link>"
             ),
             br(),
             tags$i("URBIFOR, 2019.") ),
           ## 
           p(strong("Praças Vivas"),
             a(target="_blank",
               href = "https://www.ibge.gov.br/geociencias/organizacao-do-territorio/tipologias-do-territorio/15788-aglomerados-subnormais.html",
               "<link>"
             ),
             br(),
             tags$i("AMC, 2023.") ),
           # ## 
           # p(strong("Urbanização das Zeis"),
           #   a(target="_blank",
           #     href = "https://mapas.fortaleza.ce.gov.br/api/download/shape/529",
           #     "<link>"
           #   ),
           #   br(),
           #   tags$i("IBGE, 2019.") ),
           # ## 
           
    )
  )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  })
}


#' 
#' h5(''),
#' 
#'                    choices =c('Mediação de conflitos'= 'Mediacao',
#'                               'Videomonitoramento'='cameras' ,
#'                               'Iluminação pública'='Iluminação',
#'                               'Territorios GEMP'='Territorios_GEMP',
#'                               'Territorios PMPU'='Territorios_PMPU',
#'                               'Células de proteção'='Torre'),
#'                    selected = c('')),
#' 
#' h5('Socioeconômicos'),
#' 
#'                    choices =c('Aglomerados Subnormais'='Aglomerado', 
#'                               'Assentamento Precário'='Assentamento',
#'                               'Pontos de lixo'='lixo',
#'                               'Ecopontos'='eco',
#'                               'Vínculos formais renda'='rais', RAIS/2021.
#'                               'Vazios urbanos'='Vazios',
#'                               'Gravidez na adolescência'='gravidez' SINASC - DataSUS/2022.
#'                    ),
#'                    selected = c('')),
#' 
#' h5('Infraestrutura'),
#' 
#'                    choices =c('Adoção de Áreas Verdes'='AdocaoPracasEAreasVerdes',
#'                               'Areninhas', 
#'                               'Assistência Social'='Equip_Assist_Social',
#'                               'Base Samu' = 'samu',
#'                               'Conselhos Tutelares'='Conselhos_Tutelares',
#'                               'Equipamentos de cultura' = 'cultura',
#'                               'Equipamentos de Saúde' = 'EquipamentosSaude_cnes', 
#'                               #'Escolas educação básica'='Escolas',
#'                               'Escolas municipais'='Escolas_munic',
#'                               'Praças' = 'Pracas',
#'                               'Praças Vivas' = 'Pracas_Vivas',
#'                               'Urbanização das Zeis' = 'Zeis'
#'                               