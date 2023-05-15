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
  
  desc_areninha <- read_csv('app/data/Areninhas/desc_areninha.csv',show_col_types = FALSE)
  desc_praca <- read_csv('app/data/Pracas/Praças_de_Fortaleza/desc_praca.csv',show_col_types = FALSE)
  desc_adocao <- read_csv('app/data/Pracas/Adoção_de_Praças_e_Áreas_Verdes/desc_adocao.csv',show_col_types = FALSE)
  desc_pracaviva <- read_csv('app/data/Pracas/Praças_Vivas/desc_pracaviva.csv',show_col_types = FALSE)
  desc_cons_tut <-read_csv('app/data/vw_ConselhosTutelares/desc_cons_tut.csv',show_col_types = FALSE)
  desc_social <-read_csv('app/data/vw_EquipamentosAssistenciaSocial/desc_social.csv',show_col_types = FALSE)
  desc_cult <-   read_csv('app/data/tblCultura/desc_cultura.csv',show_col_types = FALSE)
  desc_escola <- read_csv('app/data/Escolas/desc_escola.csv',show_col_types = FALSE) |> select(-Escolas)
  desc_saude <-   read_csv('app/data/202303_EstabelecimentosSaudeCNES/desc_saude.csv',show_col_types = FALSE)
  desc_samu <- read_csv('app/data/vw_BasesSAMU/desc_samu.csv',show_col_types = FALSE)

    
  


}

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico col-sm-", width), role = "main", ...)
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  tagList( 
    ############
    tags$h1("Infraestrutura",align = "center"),
    br(),
    tags$h3("Esporte e Lazer"),
    br(),
    fluidRow(
     
      column(width = 6,
             panel(
               infoBoxOutput(ns("areninhaBox"), width = 12),
               echarts4rOutput(ns('Areninha_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('Areninha'), height = 495.11)
             )),

      
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("pracasBox"), width = 12),
               echarts4rOutput(ns('pracas_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('Praca'), height = 495.11)
             )),
    ),
    ############
    hr(),
    br(),
    tags$h3("Saúde"), 
    br(),
    fluidRow(
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("saudeBox"), width = 12),
               echarts4rOutput(ns('saude_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('saude'), height = 495)
             )),
      column(width = 6,
             panel(
               infoBoxOutput(ns("samuBox"), width = 12),
               echarts4rOutput(ns('samu_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('samu'), height = 495.11)
             )),
    ),
    
    ############
    hr(),
    br(),
    tags$h3("Educação"), 
    br(),
    fluidRow(
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("educacaoBox"), width = 12),
               echarts4rOutput(ns('educacao_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('educacao'), height = 495.11)
             )),
      column(width = 6,
             panel(
               infoBoxOutput(ns("cultBox"), width = 12),
               echarts4rOutput(ns('cult_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('cult'), height = 495.11)
             ))
    ),
    
    ############
    hr(),
    br(),
    tags$h3("Assistência Social"), 
    br(),
    fluidRow(
      
      column(width = 6,
             panel(
               infoBoxOutput(ns("cons_tutBox"), width = 12),
               echarts4rOutput(ns('cons_tut_pol'), height = 380)
             )),
      column(width = 6,
             panel(
               echarts4rOutput(ns('cons_tut'), height = 495.11)
             )),
    )
    
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    e_common(theme = "walden")
    
    ## Areninha box
    output$areninhaBox <- renderInfoBox({
      infoBox(
        "Areninhas em Fortaleza",sum(rowSums(Filter(is.numeric, desc_areninha))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## Areninha
    output$Areninha <-  renderEcharts4r({
      
      desc_areninha |> 
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
    
    ## Areninha pol
    output$Areninha_pol <-  renderEcharts4r({
      
      desc_areninha[,-1] |> 
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
        #e_y_axis(name="Total",show=F) |> 
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
    
    ## pracas box
    output$pracasBox <- renderInfoBox({
      infoBox(
        "Praças e adoção de areas verdes em Fortaleza",sum(colSums(Filter(is.numeric,
                                                                          data.frame(desc_praca[,-1],desc_adocao[,c(-1,-2)],desc_pracaviva[,c(-1,-2)])))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## Praças poli
    output$pracas_pol <-  renderEcharts4r({
      
      data.frame(desc_praca[,-1],desc_adocao[,c(-1,-2)],desc_pracaviva[,c(-1,-2)]) |> 
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
    
    ## Praças
    output$Praca<-  renderEcharts4r({
      
      data.frame(desc_praca,desc_adocao[,c(-1,-2)],desc_pracaviva[,c(-1,-2)]) |> 
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
    
    ## saude box
    output$saudeBox <- renderInfoBox({
      infoBox(
        "Equipamentos de saúde em Fortaleza",sum(colSums(Filter(is.numeric,desc_saude))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## saude poli
    output$saude_pol <-  renderEcharts4r({
      
       
        df <- desc_saude[,-1] |> 
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
    
    ## saude
    output$saude<-  renderEcharts4r({
      
     
        df <-  desc_saude  |> 
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
    
    
    ## samu box
    output$samuBox <- renderInfoBox({
      infoBox(
        "Base Samu em Fortaleza",sum(colSums(Filter(is.numeric,desc_samu))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## samu poli
    output$samu_pol <-  renderEcharts4r({
      
      
      df <- desc_samu[,-1] |> 
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
    
    ## samu
    output$samu<-  renderEcharts4r({
      
      
      df <-  desc_samu  |> 
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
    
    ## social box
    output$cons_tutBox <- renderInfoBox({
      infoBox(
        "Equipamentos de Assistência Social em Fortaleza",sum(colSums(Filter(is.numeric,data.frame(desc_social[,-1],desc_cons_tut[,c(-1,-2)])))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
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
   
    ## educacao box
    output$educacaoBox <- renderInfoBox({
      infoBox(
        "Escolas municipais em Fortaleza",sum(colSums(Filter(is.numeric,desc_escola))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## educacao poli
    output$educacao_pol <-  renderEcharts4r({
      
      df <- desc_escola[,-1] |> 
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
    
    ## educacao
    output$educacao<-  renderEcharts4r({
      
      df <- desc_escola|> 
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
    
    ## cult box
    output$cultBox <- renderInfoBox({
      infoBox(
        "Equipamentos de Cultura em Fortaleza",sum(colSums(Filter(is.numeric,desc_cult))),
        icon = icon("object-align-bottom", lib = "glyphicon"),
        color = 'lightblue', fill = TRUE
      )
    })
    
    ## cult poli
    output$cult_pol <-  renderEcharts4r({
      
      df <- desc_cult[,-1] |> 
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
    
    ## cult
    output$cult<-  renderEcharts4r({
      
      df <- desc_cult|> 
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
