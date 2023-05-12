box::use(
  shiny[...],
  bs4Dash[...],
  echarts4r[e_theme_register],
  waiter[...],
  fresh[use_theme, use_googlefont]
)

box::use(
  ./mod/theme[
    diobs_theme,
    font1,
    font2,
    font3,
    main_font,
    secondary_font,
    monospace_font,
    primary
  ],
  ./view/inicio,
  ## ./view/metodologia,
  ./view/socioeconomico,
  ./view/seguranca,
  ./view/infraestrutura,
  ./view/glossario,
  ./view/fonte,
  ./view/sobre
)

#' @export
ui <- dashboardPage(
  preloader = list(
    html = tagList(
      spin_cube_grid(),
      br(),
      "Carregando ..."
    ),
    color = primary
  ),
  dark = FALSE,
  help = FALSE,
  fullscreen = TRUE,
  scrollToTop = TRUE,
  header = dashboardHeader(
    title = dashboardBrand(
      title = "Diagnóstico",
      color = "primary",
     # href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
      image = "logo.png",
      opacity = 1.0
    ),
    fixed = FALSE
  ),
  sidebar = dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    id = "sidebar",
    collapsed = TRUE,
    sidebarMenu(
      id = "current_tab",
      flat = FALSE,
      compact = FALSE,
      childIndent = TRUE,
      sidebarHeader("Visualizações"),
      menuItem(
        "Mapa",
        tabName = "inicio",
        icon = icon("map")
      ),
      menuItem(
        "Indicadores",
        #tabName = "metodologia",
        icon = icon("book"),
        
        menuSubItem("Suporte a segurança pública",
                    tabName = "seguranca",
                    icon = icon("circle")
        ),
        menuSubItem("Socioeconômicos",
          tabName = "socioeconomico",
          icon = icon("circle")
              ),
        menuSubItem("Infraestrutura",
                    tabName = "infraestrutura",
                    icon = icon("circle")
        )
        
      ),
      sidebarHeader("Metadados"),
      menuItem(
        "Glossário",
        tabName = "glossario",
        icon = icon("magnifying-glass-chart")
      ), 
      menuItem(
        "Fonte de dados",
        tabName = "fonte",
        icon = icon("server")
      ),
      menuItem(
        "Sobre",
        tabName = "sobre",
        icon = icon("info")
      )
    )
  ),
  body = dashboardBody(
    use_googlefont(font1),
    use_googlefont(font2),
    use_googlefont(font3),
    use_theme(diobs_theme),
    e_theme_register(
      paste(readLines("www/atlas_capital_humano.json"), collapse = ""),
      "diobs"),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # Listen for dark-mode messages
      tags$script("
      $(document).ready(function(){
        $('.brand-image').removeClass('elevation-3 img-circle');
      })

      Shiny.addCustomMessageHandler('dark-mode', function(dark_mode) {
        if (dark_mode) {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('dark', 'light')) });
        } else {
          $('#footer-logo').find('img').map(function() { $(this).attr('src', $(this).attr('src').replace('light', 'dark')) });
        }
      });
    "),
    tags$script(src = "https://polyfill.io/v3/polyfill.min.js?features=es6"),
    tags$script(id="MathJax-script", src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
    ),
    tabItems(
      tabItem(
        tabName = "inicio",
        inicio$ui("inicio")
      ),
      tabItem(
        tabName = "metodologia",
       # metodologia$ui("metodologia")
      ),
      tabItem(
        tabName = "seguranca",
        seguranca$ui("seguranca")
      ),
      tabItem(
        tabName = "socioeconomico",
        socioeconomico$ui("socioeconomico")
      ),
      tabItem(
        tabName = "infraestrutura",
        infraestrutura$ui("infraestrutura")
      ),
      tabItem(
        tabName = "glossario",
        glossario$ui("Glossário")
      ),
      tabItem(
        tabName = "fonte",
        fonte$ui("fonte")
      ),
      tabItem(
        tabName = "sobre",
        sobre$ui("sobre")
      )
    )
  ),
  # controlbar = dashboardControlbar(
  #   id = "controlbar",
  #   skin = "light",
  #   pinned = TRUE,
  #   overlay = FALSE,
  #     column(
  #     width = 12,
  #     inicio$ui("filtros")
  #     )
  #   # controlbarMenu(
  #   #   id = "controlbarMenu",
  #   #   type = "pills",
  #     #controlbarItem(
  #      # "Inputs",
  #    #    
  #    #      width = 12,
  #    #      align = "center",
  #    #      inicio$ui("filtros")
  #    #    )
  #    # # )
  #  # )
  # ),#dashboardControlbar(),
  footer = dashboardFooter(
    fixed = FALSE,
    left = tagList(
      # Versão mobile
      div(
        span(
          format(Sys.Date(), "%Y, "),
          a(
            href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
            target = "_blank", "Observatório de Fortaleza"
          )
        )
      )
    ),
    right = tagList(
      tags$ul(
        id = "footer-logo",
        tags$li(
          a(
            href = "https://observatoriodefortaleza.fortaleza.ce.gov.br/",
            target = "_blank",
            alt = "",
            img(
              src = "img/logo-diobs-dark.svg",
              height = "30"
            )
          )
        ),
        tags$li(
          img(
            src = "img/logo-iplanfor-dark.svg",
            height = "25"
          )
        ),
        tags$li(
          a(
            href = "https://www.fortaleza.ce.gov.br/",
            target = "_blank",
            alt = "",
            img(
              src = "img/logo-pmf-dark.svg",
              height = "35"
            )
          )
        )
      )
    )
  ),
  title = "Indicadores vulnerabilide - PreVio"
)
