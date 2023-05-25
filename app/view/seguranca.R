box::use(
  shiny[...],
  bs4Dash[...],
  echarts4r[...],
  shinyWidgets[panel],
  shinycssloaders[withSpinner],
  tidyr[drop_na,
        pivot_longer],
  dplyr[
    filter,
    group_by,
    mutate,
    rename,
    select,
    case_when,
    arrange,
    left_join,
    summarise
  ],
  naniar[replace_with_na_all],
  readr[read_csv],
  stats[aggregate]
)


#Dados
{
  
  bairros_select <- c('Barra do Ceará',	'Conjunto Ceará I','Conjunto Ceará II',
                      'Genibaú', 'Granja Portugal','Granja Lisboa','Siqueira',
                      'Canidezinho','Curió','Guajeru','Cais do Porto',
                      'Mucuripe','Vicente Pizón')
  
  
  desc_camera <- read_csv('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/desc_camera.csv',show_col_types = FALSE) |> select(-Cameras)
  desc_torre <- read_csv('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/desc_torre.csv',show_col_types = FALSE) 
  desc_mediacao <- read_csv('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/desc_mediacao.csv',show_col_types = FALSE)
  desc_pmpu <- read_csv('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/desc_pmpu.csv',show_col_types = FALSE)
  
  
}

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico col-sm-", width), role = "main", ...)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(   
    tags$h1("Suporte a segurança pública",align = "center"),
    tags$h3("Células de proteção comunitária"),
    br(),
    fluidRow(
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("pmpuBox"), width = 12),
               echarts4rOutput(ns('pmpu_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('pmpu'), height = 495.11)
             )),
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("torreBox"), width = 12),
               echarts4rOutput(ns('torre_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('torre'), height = 495.11)
             )),
      
   
      
      
       column(width = 6,
              panel(
                infoBoxOutput(ns("mediacaoBox"), width = 12),
                echarts4rOutput(ns('mediacao_pol'), height = 380)
              )),
      column(width = 6,
              panel(
                echarts4rOutput(ns('mediacao'), height = 501.58)
              ))
    ),
    ########
    hr(),
    br(),
    tags$h3("Videomonitoramento"),
    br(),
    fluidRow(
       
      column(width = 6,
             panel(
               infoBoxOutput(ns("camerasBox"), width = 12),
               echarts4rOutput(ns('cameras_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('cameras'), height = 495.11)
             )),
    # column(width = 6,
    #        panel(
    #          infoBoxOutput(ns("lixoBox"), width = 12),
    #          echarts4rOutput(ns('lixo_pol'), height = 380)
    #        )),
    # column(width = 6,
    #        panel(
    #          echarts4rOutput(ns('lixo'), height = 495.11)
    #        )),

    ),
    # #########
    # hr(),
    # br(),
    # tags$h3("Renda"), 
    # br(),
    # fluidRow(
      
      # column(width = 6,
      #        panel(
      #          infoBoxOutput(ns("cons_tutBox"), width = 12),
      #          echarts4rOutput(ns('cons_tut_pol'), height = 380)
      #        )),
      # column(width = 6,
      #        panel(
      #          echarts4rOutput(ns('cons_tut'), height = 495.11)
      #        )),
  #  )
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    e_common(theme = "walden")
    
    
    ## torre box
    output$torreBox <- renderInfoBox({
      infoBox(
        "Células de proteção comunitária em Fortaleza",sum(colSums(Filter(is.numeric,desc_torre))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'indigo', fill = TRUE
      )
    })

    ## torre poli
    output$torre_pol <-  renderEcharts4r({
      
      desc_torre[,-1] |> 
        filter(Polig_atuac != "Não") |>
        aggregate(.~Polig_atuac,FUN=sum) |> 
        pivot_longer(cols=-1) |> 
        arrange(Polig_atuac) |> 
        replace_with_na_all(condition = ~.x == 0)|> 
        group_by(name) |> 
        e_chart(x=Polig_atuac) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(
          trigger = "axis",
          axisPointer = list(
            type = "shadow"
          )) |>
        e_y_axis(show=F,
                 splitArea = list(show = FALSE),
                 splitLine = list(show = FALSE)) |> 
        e_x_axis(name="Poligonal de atuação", axisLabel = list(color = "black"),
                 nameLocation= "middle",
                 nameTextStyle= list(
                   align= "center",
                   lineHeight= 30 )) |> 
        e_toolbox_feature('dataView') |>  
        e_grid(top = "20%") |> 
        e_title(subtext ="Qtd.")
      
    }) 
    
    ## torre
    output$torre <-  renderEcharts4r({
      
      desc_torre |> 
        mutate(bairro= paste0(bairro," - ", Polig_atuac)) |> 
        filter(Polig_atuac !='Não') |> 
        arrange(Polig_atuac) |> 
        select(-2) |> 
        pivot_longer(cols=-1) |> 
        replace_with_na_all(condition = ~.x == 0)|> 
        group_by(name) |> 
        e_chart(x=bairro) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(trigger = "axis",
                  axisPointer = list(
                    type = "shadow"
                  )) |>
        e_y_axis(show=F) |>
        e_x_axis( axisLabel = list(color = "black"),inverse=T) |> 
        e_toolbox_feature('dataView') |>  
        #e_grid(left = "25%") |> 
        e_flip_coords() |> 
        e_grid(left = '25%', bottom = '0%') 
    }) 
    
    ## pmpu box
    output$pmpuBox <- renderInfoBox({
      infoBox(
        HTML("Área (km<sup>2</sup>) de atuação das céluas de proteção em Fortaleza"),round(sum(colSums(Filter(is.numeric,desc_pmpu[,'areaPol']),na.rm = T))/1000000),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'indigo', fill = TRUE
      )
    })
    
    ## pmpu poli
    output$pmpu_pol <-  renderEcharts4r({
      
      desc_pmpu[,c(-1,-5)] |> 
        #replace_with_na_all(condition = ~.x == 0)|> 
        filter(Polig_atuac != "Não") |>
        aggregate(.~Polig_atuac,FUN=sum,na.rm=TRUE, na.action=NULL) |> 
        mutate(`Área território (%)`=round(100*areaPol/areaB ,2 )) |> 
        select(1,4) |> 
        pivot_longer(cols=-1) |> 
        arrange(Polig_atuac) |> 
        replace_with_na_all(condition = ~.x == 0)|> 
        group_by(name) |> 
        e_chart(x=Polig_atuac) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(
          trigger = "axis",
          axisPointer = list(
            type = "shadow"
          )) |>
        e_y_axis(show=F,
                 splitArea = list(show = FALSE),
                 splitLine = list(show = FALSE)) |> 
        e_x_axis(name="Poligonal de atuação", axisLabel = list(color = "black"),
                 nameLocation= "middle",
                 nameTextStyle= list(
                   align= "center",
                   lineHeight= 30 )) |> 
        e_toolbox_feature('dataView') |>  
        e_grid(top = "20%") |> 
        e_title(subtext ="Qtd.")|> 
        e_color(c('#cfe49b','#3fb1e3','#6be6c1','#626c91','#a0a7e6','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))
      
    }) 
    
    ## pmpu
    output$pmpu <-  renderEcharts4r({
      
      desc_pmpu[,c(-3,-4)] |> 
        mutate(bairro= paste0(bairro," - ", Polig_atuac)) |> 
        filter(Polig_atuac !='Não') |> 
        arrange(Polig_atuac) |> 
        select(-2) |> 
        pivot_longer(cols=-1) |> 
        replace_with_na_all(condition = ~.x == 0)|> 
        group_by(name) |> 
        e_chart(x=bairro) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(trigger = "axis",
                  axisPointer = list(
                    type = "shadow"
                  )) |>
        e_y_axis(show=F) |>
        e_x_axis( axisLabel = list(color = "black"),inverse=T) |> 
        e_toolbox_feature('dataView') |>  
        #e_grid(left = "25%") |> 
        e_flip_coords() |> 
        e_grid(left = '25%', bottom = '0%') |> 
        e_color(c('#cfe49b','#3fb1e3','#6be6c1','#626c91','#a0a7e6','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))
    }) 
    
    

    ## cameras box
    output$camerasBox <- renderInfoBox({
      infoBox(
        "Câmeras de videomonitoramento em Fortaleza",sum(colSums(Filter(is.numeric,desc_camera))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'indigo', fill = TRUE
      )
    })

    ## cameras poli
    output$cameras_pol <-  renderEcharts4r({

      df <- desc_camera[,c(-1)] |>
        filter(Polig_atuac !='Não') |>
        replace_with_na_all(condition = ~.x == 0)

      df[,colSums(is.na(df))<nrow(df)]  |>
        aggregate(.~Polig_atuac,FUN=sum,na.rm=TRUE, na.action=NULL) |>
        replace_with_na_all(condition = ~.x == 0) |>
        pivot_longer(cols=-1) |>
        arrange(Polig_atuac) |>
        group_by(name) |>
        e_chart(x=Polig_atuac) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(
          trigger = "axis",
          axisPointer = list(
            type = "shadow"  )) |>
        e_y_axis(show=F,
                 splitArea = list(show = FALSE),
                 splitLine = list(show = FALSE)) |>
        e_x_axis(name="Poligonal de atuação", axisLabel = list(color = "black"),
                 nameLocation= "middle",
                 nameTextStyle= list(
                   align= "center",
                   lineHeight= 30 )) |>
        e_toolbox_feature('dataView') |>
        e_grid(top = "20%") |>
        e_title(subtext ="Qtd.") |> 
        e_color(c('#3fb1e3','#6be6c1','#626c91','#a0a7e6','#cfe49b','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))

    })

    ## cameras
    output$cameras<-  renderEcharts4r({

      df <- desc_camera |>
        filter(Polig_atuac !='Não')|>
        replace_with_na_all(condition = ~.x == 0)

      df[,colSums(is.na(df))<nrow(df)]  |>
        mutate(bairro= paste0(bairro," - ", Polig_atuac)) |>
        arrange(Polig_atuac) |>
        select(-2) |>
        pivot_longer(cols=-1) |>
        group_by(name) |>
        e_chart(x=bairro) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(trigger = "axis",
                  axisPointer = list(
                    type = "shadow"
                  )) |>
        e_y_axis(show=F) |>
        e_x_axis( axisLabel = list(color = "black"),inverse=T) |>
        e_toolbox_feature('dataView') |>
        e_flip_coords() |>
        e_grid(left = '25%', bottom = '0%') |> 
        e_color(c('#3fb1e3','#6be6c1','#626c91','#a0a7e6','#cfe49b','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))
    })
    
    ## mediacao box
    output$mediacaoBox <- renderInfoBox({
      infoBox(
        "Núcleos de mediação de conflitos em Fortaleza",sum(colSums(Filter(is.numeric,desc_mediacao))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'indigo', fill = TRUE
      )
    })
    
    ## mediacao poli
    output$mediacao_pol <-  renderEcharts4r({
      
      df <- desc_mediacao[,c(-1)] |>
        filter(Polig_atuac !='Não') |>
        replace_with_na_all(condition = ~.x == 0)
      
      df[,colSums(is.na(df))<nrow(df)]  |>
        aggregate(.~Polig_atuac,FUN=sum,na.rm=TRUE, na.action=NULL) |>
        replace_with_na_all(condition = ~.x == 0) |>
        pivot_longer(cols=-1) |>
        arrange(Polig_atuac) |>
        group_by(name) |>
        e_chart(x=Polig_atuac) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(
          trigger = "axis",
          axisPointer = list(
            type = "shadow"  )) |>
        e_y_axis(show=F,
                 splitArea = list(show = FALSE),
                 splitLine = list(show = FALSE)) |>
        e_x_axis(name="Poligonal de atuação", axisLabel = list(color = "black"),
                 nameLocation= "middle",
                 nameTextStyle= list(
                   align= "center",
                   lineHeight= 30 )) |>
        e_toolbox_feature('dataView') |>
        e_grid(top = "20%") |>
        e_title(subtext ="Qtd.") |> 
        e_color(c('#6be6c1','#3fb1e3','#626c91','#a0a7e6','#cfe49b','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))
      
    })
    
    ## mediacao
    output$mediacao<-  renderEcharts4r({
      
      df <- desc_mediacao |>
        filter(Polig_atuac !='Não')|>
        replace_with_na_all(condition = ~.x == 0)
      
      df[,colSums(is.na(df))<nrow(df)]  |>
        mutate(bairro= paste0(bairro," - ", Polig_atuac)) |>
        arrange(Polig_atuac) |>
        select(-2) |>
        pivot_longer(cols=-1) |>
        group_by(name) |>
        e_chart(x=bairro) |>
        e_bar(value,stack='g',label=list(show= T,position= "inside",color='black',fontWeight= "bold" )) |>
        e_tooltip(trigger = "axis",
                  axisPointer = list(
                    type = "shadow"
                  )) |>
        e_y_axis(show=F) |>
        e_x_axis( axisLabel = list(color = "black"),inverse=T) |>
        e_toolbox_feature('dataView') |>
        e_flip_coords() |>
        e_grid(left = '25%', bottom = '0%') |> 
        e_color(c('#6be6c1','#3fb1e3','#626c91','#a0a7e6','#cfe49b','#96dee8','#da6d6d','#af5bc6','#c69943','#894500'))
    })

    
  })
}
