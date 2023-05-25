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
  

  desc_eco <-  read_csv('app/data/vw_Ecopontos/desc_eco.csv',show_col_types = FALSE)
  desc_aglomerado <- read_csv('app/data/assentamento_ibge/desc_aglomerado.csv',show_col_types = FALSE)
  
  
   }

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico col-sm-", width), role = "main", ...)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList(   
    tags$h1("Socioeconômico",align = "center"),
      tags$h3("Habitação"),
      br(),
      fluidRow(
        
              column(width = 6,
                       panel(
                        infoBoxOutput(ns("aglomBox"), width = 12),
                        echarts4rOutput(ns('aglom_pol'), height = 380)
                      )),
              column(width = 6,
                      panel(
                       echarts4rOutput(ns('aglom'), height = 495.11)
                      )),
              
              
             #  column(width = 6,
             #         panel(
             #           infoBoxOutput(ns("vuBox"), width = 12),
             #           echarts4rOutput(ns('vu_pol'), height = 380)
             #         )),
             # column(width = 6,
             #         panel(
             #           echarts4rOutput(ns('vu'), height = 501.58)
             #         ))
      ),
      ########
      hr(),
      br(),
      tags$h3("Limpeza Urbana"), 
      br(),
      fluidRow(
          
           column(width = 6,
                    panel(
                      infoBoxOutput(ns("ecoBox"), width = 12),
                      echarts4rOutput(ns('eco_pol'), height = 380)
                    )),
             column(width = 6,
                    panel(
                      echarts4rOutput(ns('eco'), height = 495.11)
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
      #########
      hr(),
      br(),
      tags$h3("Renda"), 
      br(),
      fluidRow(
        
        # column(width = 6,
        #        panel(
        #          infoBoxOutput(ns("cons_tutBox"), width = 12),
        #          echarts4rOutput(ns('cons_tut_pol'), height = 380)
        #        )),
        # column(width = 6,
        #        panel(
        #          echarts4rOutput(ns('cons_tut'), height = 495.11)
        #        )),
      )
      
  )
    }

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    e_common(theme = "walden")

    
    
    ##  aglom box
    output$aglomBox <- renderInfoBox({
      infoBox(
        HTML("Área (km<sup>2</sup>) de aglomerados subnormais em Fortaleza"),round(sum(colSums(Filter(is.numeric,desc_aglomerado[,'area_aglom']),na.rm = T))/1000000),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'olive', fill = TRUE
      )
    })
    
    ##  aglom poli
    output$aglom_pol <-  renderEcharts4r({
      
      desc_aglomerado[,c(-1,-5)] |> 
        #replace_with_na_all(condition = ~.x == 0)|> 
        filter(Polig_atuac != "Não") |>
        aggregate(.~Polig_atuac,FUN=sum,na.rm=TRUE, na.action=NULL) |> 
        mutate(`Área território (%)`=round(100*area_aglom/areaB ,2 )) |> 
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
        e_color(c('#da6d6d','#96dee8','#626c91','#c69943','#cfe49b','#3fb1e3','#6be6c1','#a0a7e6','#96dee8','#da6d6d','#af5bc6','#894500'))
      
    }) 
    
    ## aglom
    output$aglom<-  renderEcharts4r({
      
      desc_aglomerado[,c(-3,-4)] |> 
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
        e_color(c('#da6d6d','#96dee8','#626c91','#c69943','#cfe49b','#3fb1e3','#6be6c1','#a0a7e6','#96dee8','#da6d6d','#af5bc6','#894500'))
    }) 
    
    
    ## Eco box
    output$ecoBox <- renderInfoBox({
      infoBox(
        "Ecopontos em Fortaleza",sum(colSums(Filter(is.numeric,desc_eco))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'olive', fill = TRUE
      )
    })
    
    ## Eco poli
    output$eco_pol <-  renderEcharts4r({
      
      desc_eco[,-1] |> 
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
    
    ## Eco
    output$eco<-  renderEcharts4r({
      
      desc_eco |> 
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
    
    ## social box
    output$cons_tutBox <- renderInfoBox({
      infoBox(
        "Equipamentos de Assistência Social em Fortaleza",sum(colSums(Filter(is.numeric,data.frame(desc_social[,-1],desc_cons_tut[,c(-1,-2)])))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'olive', fill = TRUE
      )
    })
    
    ## social poli
    output$cons_tut_pol <-  renderEcharts4r({
      
      df <- data.frame(desc_social[,-1],desc_cons_tut[,c(-1,-2)] ) |> 
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
        e_title(subtext ="Qtd.")
      
    }) 
    
    ## social
    output$cons_tut<-  renderEcharts4r({
      
      df <- data.frame(desc_social,desc_cons_tut[,c(-1,-2)] ) |> 
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
        e_grid(left = '25%', bottom = '0%') 
    }) 
    
    
  })
}
