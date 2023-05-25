box::use(
  shiny[...],
  shinyWidgets[...],
  bs4Dash[...],
  echarts4r[...],
  dplyr[...],
  shinyWidgets[
    multiInput,
    pickerInput
  ],
  leaflet.extras[...],
  leaflet[...],
  D3plusR[...],
  DT[...],
  data.table[last],
  shinycssloaders[withSpinner],
  stats[aggregate] 
)
Desp<- readRDS('Desp.rds')

#Rais
shp_Bairros <- readRDS('shp_Bairros_Rais.rds')
vinculos_21 <- readRDS('fort21.rds')

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
   
   
    h1("Vínculos empregatícios formais"),
    fluidRow(
      
      column(
        width = 6, offset = 3,
        #tags$h4("Filtros"),
        panel(
         
          shinyWidgets::selectizeGroupUI(
            id = ns("Filtro_rais"),
            inline = F ,
            params = list(
              Sexo = list(inputId = "Sexo", title = "Sexo", placeholder = 'Todos'),
              Raca = list(inputId = "Raça_Cor", title = "Raça/cor", placeholder = 'Todos'),
              Escolaridade = list(inputId = "Escolaridade", title = "Escolaridade", placeholder = 'Todos'),
              Faixa_etária = list(inputId = "Faixa_etária", title = "Faixa etária", placeholder = 'Todos'),
              # Estabelecimento = list(inputId = "Tamanho_Estabelecimento", title = "Tamanho do estabelecimento", placeholder = 'Todos'),
              bairro = list(inputId = "bairro", title = "Bairro", placeholder = 'Todos')
            ),
            btn_label = "Resetar filtros" ),
          actionButton(ns("add_graph"), "Gerar gráfico"),heading = "Filtros",status = "info"))
    ),
    
    fluidRow(
      box(title = 'Quantidade de vínculos ativos formais',
          #textOutput('Ano_rais'),
          solidHeader = TRUE,
          status = "primary",
          collapsible = TRUE,maximizable = TRUE,
          withSpinner(d3plusOutput(ns('treemapRAIS'),height = 600)),width =12)# ,
    #   box(title = " ", solidHeader = TRUE,status = "primary",
    #       collapsible = TRUE,
    #       withSpinner( leafletOutput(ns("mapa_rais"), height =500)),width = 12)
    # ),
    
    # tags$h3("Explorar da dados"),
    # column(width = 10, offset = 1,
    #        panel(
    #          
    #          fluidPage(
    #            checkboxGroupInput(ns("variaveis"), "Seleção de variáveis:",
    #                               choices = c('bairro', "Denom_Seção", 'Denom_Classe', #, "Denom_Divisão","Denom_Grupo",
    #                                           'Denom_Subclasse','CBO','Sexo','Raça_Cor','Escolaridade' ),
    #                               selected = c('Escolaridade' )),
    #            
    #            # Select grupo
    #            selectizeGroupUI(
    #              id = ns("rais_filters_tab"),
    #              inline = F,
    #              params = list(
    #                bairro = list(inputId = "bairro", title = "Bairro:", placeholder = 'Todos'),
    #                Denom_Seção = list(inputId = "Denom_Seção", title = "Seção:", placeholder = 'Todos'),
    #                #Denom_Divisão = list(inputId = "Denom_Divisão", title = "Divisão:"),
    #                #Denom_Grupo = list(inputId = "Denom_Grupo", title = "Grupo:"),
    #                Denom_Classe = list(inputId = "Denom_Classe", title = "Classe:", placeholder = 'Todos'),
    #                Denom_Subclasse = list(inputId = "Denom_Subclasse", title = "Subclasse:", placeholder = 'Todos'),
    #                CBO = list(inputId = "CBO", title = "CBO:", placeholder = 'Todos'),
    #                Sexo = list(inputId = "Sexo", title = "Sexo:", placeholder = 'Todos'),
    #                Raça_Cor = list(inputId = "Raça_Cor", title = "Raça/Cor:", placeholder = 'Todos'),
    #                Escolaridade = list(inputId = "Escolaridade", title = "Escolaridade:", placeholder = 'Todos'))
    #            )#Fim select grupo
    #            
    #          ),#fim fluidrows
    #          actionButton("add_table", "Gerar tabela"),
    #          heading = "Filtros",status = "info"),#Fim painel
    #        
    # ), #Fim coluna
    
    # column(width = 12,withSpinner( DT::dataTableOutput(outputId = ns("tabela_rais")))
    )




    )

    
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Filtro Rais
    {
      # filtroRais1 <- reactive({
      #   vinculos_21[ano == input$Ano_filter_rais,]
      # })
      ## treemap e mapa
      data_vinculos <-  shiny::callModule(
        module = shinyWidgets::selectizeGroupServer,
        id = "Filtro_rais",
        data = vinculos_21 ,
        vars = c('Sexo', 'Raça_Cor','Escolaridade','Faixa_etária','bairro' ), #
        inline = F
      )
      ## Tabela
      vars_rais <- reactive({
        input$variaveis
      })
      
      res_mod <- reactive({res_mod2()[, c(input$variaveis,'Vl.Remun.Média.Nom'), drop = FALSE] %>%
          group_by_all() %>%
          ungroup(Vl.Remun.Média.Nom) %>%
          summarise(Vl.Remun.Média=round(mean(Vl.Remun.Média.Nom),2),
                    Qtd.=n(),
                    .groups='keep')
        
      })
      res_mod2 <- callModule(
        module = selectizeGroupServer,
        id = "rais_filters_tab",
        data = vinculos_21 ,
        vars = vars_rais,
        inline=F
      )
      
      
    }
    
    
    
    
    {
      
      # gerar uma vez ao iniciar  
      observeEvent( data_vinculos(),{
        
        agrup_rais <-  data_vinculos() %>%
          group_by(Denom_Seção,Denom_Divisão,Denominação,CBO) %>%
          summarise(Qtd.=n(),.groups = 'keep') %>% 
          ungroup() %>% 
          mutate_if(is.character,~replace(.,is.na(.),'Não definido'))
        
        output$treemapRAIS<- renderD3plus({  
          
          d3plus(
            data = agrup_rais ,
            type = "tree_map",
            id = c("Denom_Seção", "Denom_Divisão","Denominação","CBO"),
            locale = "pt_BR",
            width = "110%",
            currency = "",
            number_text = c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões"),
            height = 500) %>%
            d3plusSize("Qtd.") %>%
            d3plusUi(list( list(method = "depth", type = "drop",
                                value = list(list('1ª Seção' = 0), list('2ª Divisão' = 1),
                                             list('3ª Denominação' = 2) )))) %>% # ,list('4ª CBO' = 3)
            
            d3plusColor("Denom_Seção") %>%
            d3plusDepth(1) %>%
            d3plusLabels(value = TRUE, valign = "top") %>%
            d3plusTitle(value = 'Ano 2021', font = list(size = 22, weight = 900),
                        total = list(value = list(prefix = "Total: "),
                                     font = list(size = 16, weight = 900))) %>%
            d3plusFooter(value = "Fonte: RAIS/MTE",
                         font = list(align = "left"))
        })
      },once=T)   
      
      # gerar quando clicar 
      observeEvent(input$add_graph,{
        agrup_rais <-  data_vinculos() %>%
          group_by(Denom_Seção,Denom_Divisão,Denominação,CBO) %>%
          summarise(Qtd.=n(),.groups = 'keep') %>% 
          ungroup() %>% 
          mutate_if(is.character,~replace(.,is.na(.),'Não definido')) 
        
        plot_rais <- d3plus(
          data =agrup_rais ,
          type = "tree_map",
          id = c("Denom_Seção", "Denom_Divisão","Denominação","CBO"),
          locale = "pt_BR",
          width = "110%",
          currency = "",
          number_text = c("Mil", "Milhão", "Milhões", "Bilhão", "Bilhões"),
          height = 500,
          clean_previous = T) %>%
          d3plusSize("Qtd.") %>%
          d3plusUi(list( list(method = "depth", type = "drop",
                              value = list(list('1ª Seção' = 0), list('2ª Divisão' = 1),
                                           list('3ª Denominação' = 2) )))) %>% # ,list('4ª CBO' = 3)
          
          d3plusColor("Denom_Seção") %>%
          d3plusDepth(1) %>%
          d3plusLabels(value = TRUE, valign = "top") %>%
          d3plusTitle(value = input$Ano_filter_rais, font = list(size = 22, weight = 900),
                      total = list(value = list(prefix = "Total: "),
                                   font = list(size = 16, weight = 900))) %>%
          d3plusFooter(value = "Fonte: RAIS/MTE",
                       font = list(align = "left"))
        
        
        output$treemapRAIS<- renderD3plus({   
          
          plot_rais
          
        })
      })
      
      # Mapa
      #Base mapa rais
      # output$mapa_rais <- renderLeaflet({
      #   req(input$menuSubItem == "socioeconomico2")
      #   leaflet(vinculos_21_shp())%>%
      #     setMapWidgetStyle(list(background= "white")) %>%
      #     # setView(-38.52782, -3.785804,zoom=11) %>% 
      #     fitBounds(-38.6379,-3.894603, -38.40133, -3.691722) %>% 
      #     addTiles(group = "Elementos do mapa") %>%
      #     #addTiles() %>% 
      #     addLayersControl(
      #       overlayGroups = c("Bairros",'Remun. média',"Elementos do mapa")) %>%
      #     hideGroup("Elementos do mapa") %>%
      #     hideGroup('Remun. média') %>%
      #     addScaleBar( position ="bottomright",
      #                  options =scaleBarOptions(imperial = F)) %>%
      #     addFullscreenControl(pseudoFullscreen = T) 
      # })
      # # 
      # #
      # data_vinc_agrup <- reactive(data_vinculos() %>%
      #                               group_by(Bairros.Fortaleza) %>%
      #                               summarise(Qtd.Vinculos=n(),
      #                                         Remun_media=mean(Vl.Remun.Média.Nom)) )
      # 
      # #Juntar com dados
      # vinculos_21_shp <- reactive(left_join(shp_Bairros,data_vinc_agrup(),by=c('cod_bairro'='Bairros.Fortaleza')) %>%
      #                               mutate(Remun_media=if_else(!is.na(cod_bairro),round(Remun_media,2), NA ),
      #                                      Qtd.Vinculos=if_else(!is.na(cod_bairro),Qtd.Vinculos, NA )))
      
      # # Mapa paleta bairros rais 
      # colorpal <- reactive ({ colorNumeric(
      #   palette = "inferno",#YlGnBu
      #   domain = vinculos_21_shp()$Remun_media,
      #   reverse = T,na.color="#D9D9D6")
      # })
      
      # PopBairro <- reactive ({  sprintf(
      #   "<strong>%s</strong></br>Remun. média: %s </br>Qtd.: %s",
      #   vinculos_21_shp()$bairro,vinculos_21_shp()$Remun_media,vinculos_21_shp()$Qtd.Vinculos)  %>%
      #     lapply(htmltools::HTML)
      # })
      
      # PopCent <- reactive ({ sprintf(
      #   "<strong>%s</strong></br> %s ",
      #   vinculos_21_shp()[!is.na(vinculos_21_shp()$bairro_rais),]$bairro,
      #   paste0("R$ ", round(vinculos_21_shp()[!is.na(vinculos_21_shp()$bairro_rais),]$Remun_media,0)) ) %>%
      #     lapply(htmltools::HTML)
      # })
      
      
      ## Polygons
      # observe({
      #   req(input$menuSubItem == "socioeconomico2")
      #   
      #   pal <- colorpal()
      #   
      #   leafletProxy("mapa_rais" ) %>%
      #     clearGroup("Bairros") %>%
      #     addPolygons(data =vinculos_21_shp(),weight=1,
      #                 opacity = 0.5,
      #                 fillColor = ~pal(Remun_media),
      #                 popup =PopBairro(),
      #                 dashArray = "1",
      #                 fillOpacity = 0.5,
      #                 group = "Bairros",
      #                 highlight=highlightOptions(
      #                   weight = 5,
      #                   color = "#666",
      #                   dashArray = "",
      #                   fillOpacity = 0.5,
      #                   bringToFront = TRUE)) 
      #   
      # })
      
      
      #labels
      # observe({
      #   req(input$menuSubItem == "socioeconomico2")
      #   leafletProxy("mapa_rais" ) %>%
      #     clearGroup("Remun. média") %>%
      #     addLabelOnlyMarkers(data =vinculos_21_shp()[!is.na(vinculos_21_shp()$bairro_rais),],
      #                         group="Remun. média",
      #                         lng = ~x, lat = ~y, label = PopCent()  ,  #~round(Remun_media_B,0),
      #                         labelOptions = labelOptions(noHide = T,  textOnly = T,
      #                                                     direction = "right",
      #                                                     style = list(
      #                                                       "color" = "black",
      #                                                       'background-color'= 'rgba(255,255,255, 0)',
      #                                                       'padding'= '0px 0px 0px 0px' ))) %>% 
      #     addCircleMarkers(data = vinculos_21_shp()[!is.na(vinculos_21_shp()$bairro_rais),],lng = ~x, lat = ~y,
      #                      radius = 2,
      #                      color = '#800500',
      #                      stroke = FALSE, fillOpacity = 1,group="Remun. média" )
      #   
      # })
      #legenda
      # observe({
      #   req(input$menuSubItem == "socioeconomico2")
      #  # req(input$sidebar == "Rais")
      #   pal <- colorpal()
      #   leafletProxy("mapa_rais",data =vinculos_21_shp() ) %>%
      #     clearControls() %>% 
      #     addLegend("bottomright", pal = pal, values = ~Remun_media,
      #               title = "Remuneração média",
      #               labFormat = labelFormat(prefix = "R$",big.mark = ' '),
      #               opacity = 1  ) 
      # })
      
      ## Tabela Rais
      
      
      #Tabela no click   
      # observeEvent(input$add_table,{
      #   
      #   dados_rais_filtrado <-  res_mod()%>% 
      #     arrange(-Vl.Remun.Média)
      #   
      #   table_filter <- DT::datatable(
      #     dados_rais_filtrado,extensions ='Buttons', options = list(
      #       buttons =list(list(
      #         extend = 'collection',
      #         buttons = c('csv', 'excel'),
      #         text = 'Download'
      #       )),
      #       #buttons = c('csv', 'excel'),
      #       lengthMenu = c(10,30),
      #       pageLength = 30,
      #       autoWidth = TRUE,
      #       dom= 'Blrtip'
      #     )) %>%  formatStyle('Vl.Remun.Média',
      #                         background = styleColorBar(c(0,res_mod()$Vl.Remun.Média), 'steelblue'),
      #                         backgroundSize = '100% 80%',
      #                         backgroundRepeat = 'no-repeat',
      #                         backgroundPosition = 'center',
      #                         fontWeight = 'bold'
      #     ) %>% formatStyle('Qtd.',
      #                       background = styleColorBar(c(0,res_mod()$Qtd.), '#F9812A'),
      #                       backgroundSize = '100% 80%',
      #                       backgroundRepeat = 'no-repeat',
      #                       backgroundPosition = 'center',
      #                       fontWeight = 'bold'
      #     )
      #   
      #   
      #   output$tabela_rais <- DT::renderDataTable(server=FALSE,{
      #     
      #     table_filter
      #   })
      # })
      
     # observeEvent(req(input$menuSubItem == "socioeconomico2"),{    
     #  #observe({  
     #    
     #    table_filter <- DT::datatable(
     #      res_mod()%>% 
     #        arrange(-Vl.Remun.Média),extensions ='Buttons', options = list(
     #          buttons =list(list(
     #            extend = 'collection',
     #            buttons = c('csv', 'excel'),
     #            text = 'Download'
     #          )),
     #          #buttons = c('csv', 'excel'),
     #          lengthMenu = c(10,30),
     #          pageLength = 30,
     #          autoWidth = TRUE,
     #          dom= 'Blrtip'
     #        )) %>%  formatStyle('Vl.Remun.Média',
     #                            background = styleColorBar(c(0,res_mod()$Vl.Remun.Média), 'steelblue'),
     #                            backgroundSize = '100% 80%',
     #                            backgroundRepeat = 'no-repeat',
     #                            backgroundPosition = 'center',
     #                            fontWeight = 'bold'
     #        ) %>% formatStyle('Qtd.',
     #                          background = styleColorBar(c(0,res_mod()$Qtd.), '#F9812A'),
     #                          backgroundSize = '100% 80%',
     #                          backgroundRepeat = 'no-repeat',
     #                          backgroundPosition = 'center',
     #                          fontWeight = 'bold'
     #        )
     #    output$tabela_rais <- DT::renderDataTable(server=FALSE,{
     #      
     #      table_filter
     #    })
     #    
     #  #})
     #  },once=T)
      
    }
    
    
  })
}