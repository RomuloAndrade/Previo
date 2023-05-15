box::use(
  shiny[...],
  shinyWidgets[...],
  bs4Dash[...],
  dplyr[
    filter,
    group_by,
    mutate,
    rename,
    select,
    case_when,
    arrange,
    left_join
  ],
  naniar[replace_with_na_all],
  stringr[str_to_title],
  leaflet[leafletProxy,
          addMapPane,
          setView,
          pathOptions,
          colorFactor,
          layersControlOptions,
          clearControls,
          clearGroup,
          leaflet,
          addCircleMarkers,
          removeMeasure,
          labelOptions,
          providers,
          renderLeaflet,
          leafletOutput,
          addMeasure,
          addLabelOnlyMarkers,
          addPolygons,
          highlightOptions,
          addScaleBar,
          addLegend,
          scaleBarOptions,
          hideGroup,
          addLayersControl,
          addProviderTiles],
  leaflet.extras[addFullscreenControl,
                 setMapWidgetStyle],
  leaflegend[addSymbols],
  leafpop[popupTable],
  readr[read_csv],
  sf[st_read,
     read_sf,
     st_zm,st_drop_geometry,
     st_transform, st_cast,st_as_sf],
  readxl[read_xlsx],
  shinyWidgets[
    multiInput,
    pickerInput
  ]
)

filterPanel <- function (..., width = 3)
{
  div(class = paste0("filtro mx-2 p-3 col-sm-", width),
      tags$form(class = "well", 
                role = "complementary", ...))
}

plotPanel <- function (..., width = 8)
{
  div(class = paste0("grafico col-sm-", width), role = "main", ...)
}



#Leitura dados
{

  # Bairros
  # shp_Bairros <- readRDS('app/data/shp_Bairros/shp_BairrosA.rds')|> 
  #   select(-bairro_rais,-cod_bairro)

    shp_Bairros <- st_read('app/data/shp_Bairros/shp_BairrosA.shp',quiet =T)
  
  
  nome_bairro<- read_csv('app/data/nomes_bairros.csv',show_col_types = FALSE)
  # AssentamentoPrecarios
  
  Assentamento  <- st_read('app/data/vw_AssentamentoPrecarios/vw_AssentamentoPrecarios.shp',
                           options = "ENCODING=WINDOWS-1252",quiet =T)   |> 
    select(bairro,tipo,area_total,dominio,vias_pavim,nome_assen) |> 
    mutate(vias_pavim=case_when(vias_pavim=='não'~'Não pavimentada',
                                vias_pavim=='total'~'Pavimentada',
                                vias_pavim=='parcial'~'Parcialemente pavimentada'),
           tipo=str_to_title(tipo))
  
  # Aglomerados subnormais ibge
  Aglomerado <- st_read('app/data/assentamento_ibge/Aglomerado.shp',quiet =T) |> 
    st_transform(4326)
  desc_aglomerado <- read_csv('app/data/assentamento_ibge/desc_aglomerado.csv',show_col_types = FALSE)[,c(1,5)]
  # Areninhas
  Areninhas <- read_sf('app/data/Areninhas/vw_Areninhas.shp',options = "ENCODING=WINDOWS-1252",quiet =T)
  desc_areninha <- read_csv('app/data/Areninhas/desc_areninha.csv',show_col_types = FALSE)[,-2]
  colAreninhas<- colorFactor("viridis", Areninhas$tipo)
  
  eco <-  read_sf('app/data/vw_Ecopontos/vw_Ecopontos.shp',
    options = "ENCODING=WINDOWS-1252" ,quiet =T)
  
  desc_eco <-  read_csv('app/data/vw_Ecopontos/desc_eco.csv',show_col_types = FALSE)[,-2]
  
  # Praças
  Pracas <- read_sf('app/data/Pracas/Praças_de_Fortaleza/vw_PracasFortaleza.shp',
                    options = "ENCODING=WINDOWS-1252",quiet =T) 
  desc_praca <- read_csv('app/data/Pracas/Praças_de_Fortaleza/desc_praca.csv',show_col_types = FALSE)[,-2]
  # Adoçao Praças e Areas Verdes
  AdocaoPracasEAreasVerdes <- read_sf('app/data/Pracas/Adoção_de_Praças_e_Áreas_Verdes/vw_AdocaoPracasEAreasVerdes.shp',
                                      options = "ENCODING=WINDOWS-1252",quiet =T)
  desc_adocao <- read_csv('app/data/Pracas/Adoção_de_Praças_e_Áreas_Verdes/desc_adocao.csv',show_col_types = FALSE)[,-2]
  # Urbanização_Praça_Moura_Brasil
  Urbanização_Praça_Moura_Brasil<- read_sf('app/data/Pracas/Urbanização_de_Praça_-_Zeis_Moura_Brasil/vw_urbanizacao_praca_mourabrasil.shp',
                                           options = "ENCODING=WINDOWS-1252",quiet =T) |> 
    st_zm( drop = TRUE, what = "ZM") |> 
    mutate(Zeis='Moura Brasil') |> 
    select(Zeis)
  
  # Praças_Zeis_lagamar
  pracas_lagamar<- read_sf('app/data/Pracas/Praças_-_Zeis_Lagamar/vw_pracas_lagamar.shp',
                           options = "ENCODING=WINDOWS-1252",quiet =T) |> 
    mutate(Zeis='Lagamar')|> 
    select(Zeis)
  
  Zeis <- rbind(Urbanização_Praça_Moura_Brasil,pracas_lagamar)
  colZeis <- colorFactor('RdGy', Zeis$Zeis) 
  # Praças_Vivas
  Pracas_Vivas<- read_sf('app/data/Pracas/Praças_Vivas/vw_PracasVivas.shp', 
                         options = "ENCODING=WINDOWS-1252",quiet =T)  
  
  desc_pracaviva <- read_csv('app/data/Pracas/Praças_Vivas/desc_pracaviva.csv',show_col_types = FALSE)[,-2]
  
  # Equip Saúde
  # EquipamentosSaude <- st_read('app/data/vw_EquipamentosSaude/vw_EquipamentosSaude.shp',
  #                              options = "ENCODING=WINDOWS-1252",quiet =T) 
 
  # atencao_primaria <- st_read('estabelecimentos_atencao_primaria/estabelecimentos_atencao_primaria_fortaleza.shp') |> 
  #   st_transform(4326) |> 
  #   mutate(tipo='Unidade de Atenção Primária à Saúde') |> 
  #   rename(nome='AP_NO_FAN')
  # 
  # internacao_observacao <- st_read('internacao_observacao/estabelecimentos_com_suporte_internacao_observacao_fortaleza.shp') |> 
  #   st_transform(4326) |> 
  #   mutate(tipo='Unidade de Internação/Observação')|> 
  #   rename(nome='UH_NOME')
  # 
  # basecnes <- rbind(atencao_primaria,internacao_observacao) 
  # sf::st_write(basecnes, dsn = "basecnes.geojson", layer = "basecnes.geojson")
  
  EquipamentosSaude_cnes <- read_sf('app/data/202303_EstabelecimentosSaudeCNES/saude.shp',quiet =T)
  colEquipamentosSaude_cnes <- colorFactor("Spectral", EquipamentosSaude_cnes$tipo) 
  
  desc_saude <-   read_csv('app/data/202303_EstabelecimentosSaudeCNES/desc_saude.csv',show_col_types = FALSE)[,-2]
  # PopEquipamentosSaude <- sprintf(
  #   "<strong>%s</strong> </br> Esfera: %s  " ,
  #   EquipamentosSaude$nome_fanta,EquipamentosSaude$esfera  
  # )  |>lapply(htmltools::HTML)
  
  #  Equip Cultura
  Equip_cult <- st_read('app/data/tblCultura/Equip_cult.shp',quiet =T) |> 
    st_cast("POINT") |> 
    mutate(tipo=case_when(tipo=='Biblioteca Pública'~'Biblioteca',
                          tipo=='Centros Urbanos de Cultura, Arte, Ciência e Esporte'~'CUCA',
                          TRUE ~tipo))
  
  colcultura <- colorFactor('Set3', Equip_cult$tipo) 
  
  desc_cult <-   read_csv('app/data/tblCultura/desc_cultura.csv',show_col_types = FALSE)[,-2]
  
  #Conselho tutelar
  
  Conselhos_Tutelares <- st_read('app/data/vw_ConselhosTutelares/vw_ConselhosTutelares.shp',
                                 options = "ENCODING=WINDOWS-1252",quiet =T) |> 
    st_cast("POINT")
  
  desc_cons_tut <- read_csv('app/data/vw_ConselhosTutelares/desc_cons_tut.csv',show_col_types = FALSE)[,-2]
  
  #Mediação
  Mediacao <- st_read('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/Mediacao_conflitos.shp',quiet =T) 
  desc_mediacao <- read_csv('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/desc_mediacao.csv',show_col_types = FALSE)[,-2]
  
  # BasesPMPU
  Torre <-  read_sf('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/Bases_PMPU.shp',quiet =T)
  desc_torre <- read_csv('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/desc_torre.csv',show_col_types = FALSE)[,-2]
  # Território PMPU
  Territorios_PMPU <- read_sf('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/Territorios_PMPU.shp')
  desc_pmpu <- read_csv('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/desc_pmpu.csv',show_col_types = FALSE)[,c(-2,-3,-4)] |> 
  rename(`Área território pmpu (%)`=`Área território (%)`)
  # Território GEMP
  Territorios_GEMP <- read_sf('app/data/Sesec/202304MicroterritoriosGEMPGMFNOVO/Microterritorios_GEMP_GMF.shp')  
  
  #Escolas
  Escolas <- readRDS('app/data/Escolas/Pontos_escolas.rds')
  colEscolas <- colorFactor("Set1", Escolas$government_level)
  
  Escolas_munic <-  read_sf('app/data/Escolas/escolas.shp') 
  colEscolas_munic <- colorFactor("Dark2", Escolas_munic$tipo) 
  
  desc_escola <- read_csv('app/data/Escolas/desc_escola.csv',show_col_types = FALSE)[,-2]
  
  # Câmeras 
  Cameras <- st_read('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_geral.shp',quiet =T) 
  colCameras <- colorFactor('Dark2', Cameras$Órgão) 
  desc_camera <- read_csv('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/desc_camera.csv',show_col_types = FALSE)[,-2]
  
  
  Samu  <- st_read('app/data/vw_BasesSAMU/vw_BasesSAMU.shp',options = "ENCODING=WINDOWS-1252",quiet =T) 
  
  desc_samu <- read_csv('app/data/vw_BasesSAMU/desc_samu.csv',show_col_types = FALSE)[,-2]
  # Atuação previo
  shp_atuacao <- st_read('app/data/poligonais_atuacao/poligonais_atuacao.shp',quiet =T)  |> 
    select(bairro) |> 
    st_transform(4326)  |> 
    mutate(Poligono=c('Poligonal 3','Poligonal 1', 'Poligonal 2', 'Poligonal 4', 'Poligonal 5', 'Poligonal 6')) |> 
    arrange(Poligono) |>
    select(Poligono) |> 
    mutate(col=c('#C4DD81','#FFEE6A','#39DDB2','#DCAEE9','#FEB95C','#93B5EC'))
  
  
  Equip_Assist_Social <- st_read('app/data/vw_EquipamentosAssistenciaSocial/vw_EquipamentosAssistenciaSocial.shp',
                                 options = "ENCODING=WINDOWS-1252",quiet =T)  |>  
    select(nome,tipo,fone)  |>  
    st_cast("POINT") |> 
    mutate(tipo=case_when(
      tipo=="Unidade de Acolhimento Pousada Social" ~ "Unidade de Acolhimento",
      tipo=="Unidade de Acolhimento Institucional para Crianças e Adolescentes" ~ "Unidade de Acolhimento",
      tipo=="Unidade de Acolhimento Pousada Social"~'Pousada Social',
      TRUE ~ tipo  
    ))
  
  desc_social <- read_csv('app/data/vw_EquipamentosAssistenciaSocial/desc_social.csv',show_col_types = FALSE)[,-2]
  
  colEquip_Assist_Social <- colorFactor("Greens", Equip_Assist_Social$tipo)
  
  Descritiva <- read_csv('app/data/Descritiva/Descritiva.csv',show_col_types = FALSE) |>
    filter(Polig_atuac!="Não")
  
  
}



#' @export
ui <- function(id) {
  ns <- NS(id)

    #tagList(   
   #   tags$h3("Explorar dados"),
      fluidRow(
        column(width = 4, offset = 0,
               panel( fluidPage(  h4('Seleção de camadas',align = "center") ),
                 fluidRow(
                 column(width = 6,
                        
                        div(
          
                        h5('Suporte a segurança pública'),
                        checkboxGroupInput(ns("segurança"), "",
                                             choices =c('Mediação de conflitos'= 'Mediacao',
                                                        'Videomonitoramento'='cameras' ,
                                                        'Iluminação pública*'='Iluminação',
                                                        'Territorios GEMP'='Territorios_GEMP',
                                                        'Territorios PMPU'='Territorios_PMPU',
                                                        'Células de proteção'='Torre'),
                                             selected = c('')),
                          
                        h5('Socioeconômicos'),
                        checkboxGroupInput(ns("socioeconomica"), "",
                                           choices =c('Aglomerados Subnormais'='Aglomerado', 
                                                      'Assentamento Precário'='Assentamento',
                                                      'Pontos de lixo*'='lixo',
                                                      'Ecopontos'='eco',
                                                      'Vínculos formais renda*'='rais',
                                                      'Vazios urbanos*'='Vazios',
                                                      'Gravidez na adolescência*'='gravidez'
                                                      ),
                                           selected = c('')),
                  
                        h5('Infraestrutura'),
                        checkboxGroupInput(ns("infraestrutura"), "",
                                           choices =c('Adoção de Áreas Verdes'='AdocaoPracasEAreasVerdes',
                                                      'Areninhas', 
                                                      'Assistência Social'='Equip_Assist_Social',
                                                      'Base Samu' = 'samu',
                                                      'Conselhos Tutelares'='Conselhos_Tutelares',
                                                      'Equipamentos de cultura' = 'cultura',
                                                      'Equipamentos de Saúde' = 'EquipamentosSaude_cnes', 
                                                      #'Escolas educação básica'='Escolas',
                                                      'Escolas municipais'='Escolas_munic',
                                                      'Praças' = 'Pracas',
                                                      'Praças Vivas' = 'Pracas_Vivas',
                                                      'Urbanização das Zeis' = 'Zeis'
                                                      ),
                                           selected = c('')),
                        
                        style="font-size:90%" ) ,
                        
                        
                        heading = "Filtros",status = "info"),
                 
                 
                 column(width = 6, 
                        
                        div(actionButton(ns("add_mapa"), "Gerar mapa"),
                           # actionButton(ns("add_save"), "Salvar mapa"),
                            style = "margin-bottom:-12px;text-align:center" ),
                        
                        fluidPage(
                          
                        div(  
                          uiOutput(ns("CamerasControls")),
                          uiOutput(ns("AglomeradoControls")),
                          uiOutput(ns("AssentamentoControls")) ,
                          uiOutput(ns("AreninhasControls")),
                          uiOutput(ns("Equip_Assist_SocialControls")),
                          uiOutput(ns("CulturaControls")),
                          uiOutput(ns("EquipamentosSaudeControls")),
                          #uiOutput(ns("EscolasControls")),
                          uiOutput(ns("Escolas_municControls"))
                    ,style="font-size:80%; line-height: .8"))
                 )
                 )#,Fim painel
                 
               )
        ), #Fim coluna
        column(width =8,
               leafletOutput(ns("mapa_base"), height = '85vh'),
               p('* Em construção.',align = 'right')
        )
      )
    
 
    
     
      
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
   ns <- NS(id)
   
   output$AssentamentoControls <- renderUI({
     conditionalPanel(
      ns = ns,
     hr(),
      condition = "input.socioeconomica.includes('Assentamento')",
     h6("Assentamento precário"),
     selectizeGroupUI(
       id = ns("Assentamento-filters"),
       inline = F ,
       params = list(
         tipo = list(
           inputId = "tipo",
           title = "Tipo assentamento:",
           placeholder = 'Todos'
         ),
         vias_pavim = list(
           inputId = "vias_pavim",
           title = "Via:",
           placeholder = 'Todos'
         )
       ),
       btn_label = "Resetar filtros"
     ) )
     
   })
   
   output$Equip_Assist_SocialControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.infraestrutura.includes('Equip_Assist_Social')",
       h6("Assistência Social"),
       selectizeGroupUI(
         id = ns("Equip_Assist_Social-filters"),
         inline = F ,
        params = list(
           tipo = list(
             inputId = "tipo",
             title = "Tipo de equipamento:",
             placeholder = 'Todos'
           )
         ),
         btn_label = "Resetar filtros"
       )
     )
   })
   
   # output$EscolasControls <- renderUI({
   #   conditionalPanel(
   #     ns = ns,
   #     hr(),
   #     condition = "input.infraestrutura.includes('Escolas')",
   #     h6("Escolas"),
   #     selectizeGroupUI(
   #       id = ns("Escolas-filters"),
   #       inline = F ,
   #       params = list(
   #         government_level = list(
   #           inputId = "government_level",
   #           title = "Nível de governo:",
   #           placeholder = 'Todos'
   #         )
   #       ),
   #       btn_label = "Resetar filtros"
   #     ),
   #   )
   # })
   
   output$Escolas_municControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.infraestrutura.includes('Escolas_munic')",
       h6("Escolas municipais"),
       selectizeGroupUI(
         id = ns("Escolas_munic-filters"),
         inline = F ,
         params = list(
           tipo = list(
             inputId = "tipo",
             title = "Tipo:",
             placeholder = 'Todos'
           )
         ),
         btn_label = "Resetar filtros"
       ),
     )
   })
   
   output$EquipamentosSaudeControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.infraestrutura.includes('EquipamentosSaude_cnes')",
       h6("Equipamentos de saúde"),
       selectizeGroupUI(
         id = ns("EquipamentosSaude_cnes-filters"),
         inline = F ,
         params = list(
           tipo = list(inputId = "tipo", title = "Tipo de equipamento:", placeholder = 'Todos')
           ),
         btn_label = "Resetar filtros"
       ),
     )
   })
   
   output$CamerasControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.segurança.includes('cameras')",
       h6("Videomonitoramento"),
       selectizeGroupUI(
         id = ns("cameras-filters"),
         inline = F ,
         params = list(
           Órgão = list(inputId = "Órgão", title = "Órgão:", placeholder = 'Todos')#,
           # Tipo = list(inputId = "Tipo", title = "Tipo:", placeholder = 'Todos')
         ),
         btn_label = "Resetar filtros"
       ),
     )
   })   
  
   output$CulturaControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.infraestrutura.includes('cultura')",
       h6("Equipamentos de cultura"),
       selectizeGroupUI(
         id = ns("cultura-filters"),
         inline = F ,
         params = list(
           tipo = list(inputId = "tipo", title = "Tipo de equipamento:", placeholder = 'Todos')
         ),
         btn_label = "Resetar filtros"
       ),
     )
   })   
  
   output$AreninhasControls <- renderUI({
     conditionalPanel(
       ns = ns,
       hr(),
       condition = "input.infraestrutura.includes('Areninhas')",
       h6("Areninhas"),
       selectizeGroupUI(
         id = ns("Areninhas-filters"),
         inline = F ,
         params = list(
           tipo = list(inputId = "tipo", title = "Tipo:", placeholder = 'Todos')
         ),
         btn_label = "Resetar filtros"
       ),
     )
   })  
  

   #callModule    
   {

    ######## Assentamento-filters
    AssentamentoR <- callModule(
      module = selectizeGroupServer,
      id = "Assentamento-filters",
      data = Assentamento,
      vars = c("tipo", "vias_pavim"),
      inline = F
    )
   
    ######## Equip_Assist_Social-filters
    Equip_Assist_SocialR <- callModule(
      module = selectizeGroupServer,
      id = "Equip_Assist_Social-filters",
      data = Equip_Assist_Social,
      vars = c("tipo"),
      inline = F
    )

    ######## Escola-filters
    # EscolasR <- callModule(
    #   module = selectizeGroupServer,
    #   id = "Escolas-filters",
    #   data = Escolas,
    #   vars = c("government_level"),
    #   inline = F
    # )
    
    Escolas_municR <- callModule(
      module = selectizeGroupServer,
      id = "Escolas_munic-filters",
      data = Escolas_munic,
      vars = c("tipo"),
      inline = F
    )

    ######## Equip_Saude_filters
    # SaudeR <- callModule(
    #   module = selectizeGroupServer,
    #   id = "EquipamentosSaude-filters",
    #   data = EquipamentosSaude,
    #   vars = c("esfera"),
    #   inline = F
    # )

    ######## Equip_Saude_filters
    Saude_cnesR <- callModule(
      module = selectizeGroupServer,
      id = "EquipamentosSaude_cnes-filters",
      data = EquipamentosSaude_cnes,
      vars = c("tipo"),
      inline = F
    )

    ######## Equip_Saude_filters
    SamuR <- callModule(
      module = selectizeGroupServer,
      id = "samu-filters",
      data = Samu,
      vars = c("status"),
      inline = F
    )

    ######## cameras_AMC_filters
    CamerasR <- callModule(
      module = selectizeGroupServer,
      id = "cameras-filters",
      data = Cameras,
      vars = c("Órgão"),
      inline = F
    )

    ######## Areninhas_filters
    AreninhasR <- callModule(
      module = selectizeGroupServer,
      id = "Areninhas-filters",
      data = Areninhas,
      vars = c("tipo"),
      inline = F
    )
    
    ######## Zeis_filters
    ZeisR <- callModule(
      module = selectizeGroupServer,
      id = "Zeis-filters",
      data = Zeis,
      vars = c("Zeis"),
      inline = F
    )
    
    ######## cultura_filters
    culturaR <- callModule(
      module = selectizeGroupServer,
      id = "cultura-filters",
      data = Equip_cult,
      vars = c("tipo"),
      inline = F
    )
   }
    
   
   descrit <-   shp_Bairros |>
     st_drop_geometry() |>
     select(bairro)
   
 
   
    # BASE MAPA
    output$mapa_base <- renderLeaflet({
      
    
      
      leaflet() |>
        setMapWidgetStyle(list(background = "white"))  |> 
        addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron")  |>
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap")  |> 
        addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter")  |>
        addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery")  |>
        addLayersControl(
          baseGroups = c("CartoDB.Positron","OpenStreetMap",'CartoDB.DarkMatter','WorldImagery'),
          overlayGroups = c(
            "Elementos do mapa",
            "Bairros",
            "Área de Atuação",
            'Nomes dos bairros')
        ) |>
        hideGroup("Elementos do mapa")  |>
        hideGroup('Nomes dos bairros') |>
        addScaleBar(position = "bottomright",
                    options = scaleBarOptions(imperial = F))  |>
        setView(-38.52782, -3.785804, zoom = 12) |>
        addFullscreenControl(pseudoFullscreen = T)|> 
        addPolygons(data=shp_atuacao,
                    weight=1,
                    opacity = 0.5,
                    fillColor = shp_atuacao$col,
                    # popup = PopAtuac() ,
                    dashArray = "0",
                    fillOpacity = 0.6,
                    group = 'Área de Atuação',
                    highlight=highlightOptions(
                      weight = 5,
                      color = "#865",
                      dashArray = "",
                      fillOpacity = 0.5,
                      bringToFront = F) )|> 
        addPolygons(data=shp_Bairros,
                    weight=1,
                    opacity = 0.5,
                    fillColor = "white",
                    popup =  ~bairro,
                    dashArray = "0",
                    fillOpacity = 0,
                    group = "Bairros"
        )  |>
      addLabelOnlyMarkers(data = shp_Bairros,group='Nomes dos bairros',
                            lng = ~x, lat = ~y, label = ~shp_Bairros$bairro  ,
                            labelOptions = labelOptions(noHide = T,  textOnly = T,
                                                        direction = "right",
                                                        style = list(
                                                          "color" = "black",
                                                          'background-color'= 'rgba(255,255,255, 0)',
                                                          'padding'= '0px 0px 0px 0px'      ))) |> 
        addMeasure(
          position = "topright",
          primaryLengthUnit = "meters",
          primaryAreaUnit = "sqmeters",
          secondaryLengthUnit = "kilometers",
          activeColor = "#3D535D",
          completedColor = "#7D4479",
          localization = "pt_BR",
          captureZIndex = 10000
        )|> 
       addMapPane("pontos", zIndex = 420) |>
       addMapPane("poligono", zIndex = 415) |>
       addMapPane("bairros",zIndex = 410)
      
      
    })
    # 
    observeEvent(input$add_mapa,{ 
      
      descrit_pop <- descrit
      
      leafletProxy("mapa_base" )  |> 
        clearGroup(c("id_Escolas","id_Assentamento","id_Social","id_Saude",
                     "id_Aglomerado",'id_Saude_cnes','id_Conselhos_Tutelares',
                     'id_samu','id_cameras',"id_Areninhas","id_AdocaoPracasEAreasVerdes",
                     'id_mediacao','id_Zeis','id_Pracas','id_Pracas_Vivas','id_cultural',
                     'id_eco','id_Territorios_PMPU','id_Territorios_GEMP','id_torre',
                     'id_Escolas_munic')) |> 
        clearControls() |> 
        removeMeasure()
      
      #if("Escolas" %in% input$infraestrutura  )
      {
        # leafletProxy("mapa_base")  |>
        #   addSymbols(data=EscolasR(),lng=~lng,lat=~lat,
        #              #clusterOptions = markerClusterOptions(maxClusterRadius = 10),
        #              group = "id_Escolas" ,
        #              width = 5,
        #              color = colEscolas(EscolasR()$government_level),
        #              popup = popupTable(EscolasR() ,
        #                                 zcol = c("name_muni","education_level", "government_level","Matric"),
        #                                 feature.id = FALSE,
        #                                 row.numbers = FALSE) ,
        #              fillOpacity = 1 )|>
        #   addLegend("bottomright", pal = colEscolas,
        #             values = EscolasR()$government_level,
        #             title = "Escolas",
        #             opacity = 1)
      }
      if("Escolas_munic" %in% input$infraestrutura  )
      {

        
        leafletProxy("mapa_base")  |>
          addSymbols(data=Escolas_municR(),lng=~lon,lat=~lat,
                     #clusterOptions = markerClusterOptions(maxClusterRadius = 10),
                     group = "id_Escolas_munic" ,
                     width = 5,
                     color = colEscolas_munic(Escolas_municR()$tipo),
                     popup = popupTable(Escolas_municR() ,
                                        zcol = c("nome","tipo"),
                                        feature.id = FALSE,
                                        row.numbers = FALSE) ,
                     fillOpacity = 1,
                     options = pathOptions(pane = "pontos"))|>
          addLegend("bottomright", pal = colEscolas_munic,
                    values = Escolas_municR()$tipo,
                    title = "Escolas municipais",
                    opacity = 1)
        

        descrit_pop <- left_join(descrit_pop,
                                 if(is.null(input[["Escolas_munic-filters-tipo"]]))
                                 {desc_escola[,c('bairro','Escolas')] } else 
                                 {select(desc_escola[,-8],bairro,!!input[["Escolas_munic-filters-tipo"]]) }  ,
                                 by=c('bairro'='bairro') )
      }
      
      if("Assentamento" %in% input$socioeconomica  )
      {
        leafletProxy(mapId="mapa_base")  |>
          addPolygons(data=st_as_sf(AssentamentoR()),
                      weight=1,
                      opacity = 0.5,
                      #fillColor = Assentamento$col,
                      popup = ~nome_assen ,
                      dashArray = "1",
                      fillOpacity = 0.5,
                      group = 'id_Assentamento',
                      highlight=highlightOptions(
                        weight = 5,
                        color = "#865",
                        dashArray = "",
                        fillOpacity = 0.5,
                        bringToFront = F))|> 
          addLegend("bottomright",colors =  "#03F",
                    labels =c('Assentamento precário - F2040'),
                    opacity = 1)
      }
      
      if("Aglomerado" %in% input$socioeconomica  )
      {
        leafletProxy(mapId="mapa_base")  |>
          addPolygons(data=Aglomerado,
                      weight=1,
                      opacity = 0.5,
                      fillColor = 'red',
                      color='red4',
                      popup = popupTable(Aglomerado ,
                                         zcol = c("nome","area"),
                                         feature.id = FALSE,
                                         row.numbers = FALSE) ,
                      dashArray = "1",
                      fillOpacity = 0.5,
                      group = 'id_Aglomerado',
                      options = pathOptions(pane = "poligono"),
                      highlight=highlightOptions(
                        weight = 5,
                        color = "#865",
                        dashArray = "",
                        fillOpacity = 0.5,
                        bringToFront = F))|> 
          addLegend("bottomright",colors =  "red",
                    labels =c('Aglomerado subnormal - IBGE'),
                    opacity = 1)
        descrit_pop <- left_join(descrit_pop,desc_aglomerado,by=c('bairro'='bairro') )
        
      }
      
      if('Equip_Assist_Social' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          # clearGroup(group='social') |>
          addCircleMarkers(data = st_as_sf(Equip_Assist_SocialR()),
                           group='id_Social',
                           popup = ~nome,
                           weight = 1,
                           radius =4,
                           color = 'purple',
                           fillColor = colEquip_Assist_Social(Equip_Assist_SocialR()$tipo),
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")) |> 
          addLegend("bottomright", pal = colEquip_Assist_Social,
                    values = Equip_Assist_SocialR()$tipo,
                    title = "Equipamentos de Assist. Social",
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,
                  if(is.null(input[["Equip_Assist_Social-filters-tipo"]]))
                  {desc_social } else 
                  {select(desc_social,bairro,!!input[["Equip_Assist_Social-filters-tipo"]]) }  ,
                  by=c('bairro'='bairro') )
      }
      
      if('EquipamentosSaude_cnes' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          # clearGroup(group='social') |>
          addCircleMarkers(data = st_as_sf(Saude_cnesR()),
                           group='id_Saude_cnes',
                           popup = popupTable(Saude_cnesR() ,
                                              zcol = c('nome','tipo'),
                                              feature.id = FALSE,
                                              row.numbers = FALSE) , 
                           weight = 1,
                           radius =4,
                           fillColor = colEquipamentosSaude_cnes(Saude_cnesR()$tipo),
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")) |>
          addLegend("bottomright", pal = colEquipamentosSaude_cnes,
                    values = Saude_cnesR()$tipo,
                    title = "Equipamentos de Saúde - Cnes",
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,
                  if(is.null(input[["EquipamentosSaude_cnes-filters-tipo"]]))
                  {desc_saude } else 
                  {select(desc_saude,bairro,!!input[["EquipamentosSaude_cnes-filters-tipo"]]) }  ,
                  by=c('bairro'='bairro') )
        
      }
      
      if('Conselhos_Tutelares' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          # clearGroup(group='social') |>
          addCircleMarkers(data = Conselhos_Tutelares,
                           group='id_Conselhos_Tutelares',
                           popup = ~nome,
                           weight = 2,
                           fillColor = 'pink',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos"))|>
          addLegend("bottomright",colors =  'pink',
                    labels =c('Conselhos Tutelares'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_cons_tut,by=c('bairro'='bairro') )
      }
      
      if('samu' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          # clearGroup(group='social') |>
          addCircleMarkers(data = st_as_sf(SamuR()),
                           group='id_samu',
                           popup = ~base_id,
                           weight = 2,
                           fillColor = 'yellow',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")) |>
          addLegend("bottomright",colors =  'yellow',
                    labels =c('Base Samu'),
                    opacity = 1)
        descrit_pop <- left_join(descrit_pop,desc_samu ,by=c('bairro'='bairro'))
        
      }
      
      if('Mediacao' %in% input$segurança )
      {
        leafletProxy("mapa_base")  |>
          # clearGroup(group='social') |>
          addCircleMarkers(data = Mediacao,
                           group='id_mediacao',
                           popup = ~Nome_Abrev,
                           weight = 2,
                           fillColor = 'brown',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")) |>
          addLegend("bottomright",colors =  'brown',
                    labels =c('Núcleo de mediação de conflitos'),
                    opacity = 1)
        descrit_pop <- left_join(descrit_pop,desc_mediacao ,by=c('bairro'='bairro'))
        
      }
      
      if('cameras' %in% input$segurança )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = st_as_sf(CamerasR()),
                           group='id_cameras',
                           popup = ~Órgão,
                           weight = 1,
                           radius =3.5,
                           color='#F5CE33',
                           fillColor = colCameras(CamerasR()$Órgão)  , #'yellowgreen',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")) |>
          addLegend("bottomright", pal = colCameras,
                    values = CamerasR()$Órgão,
                    title = "Videomonitoramento",
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,
                  if(is.null(input[["cameras-filters-Órgão"]]))
                  {desc_camera[,c('bairro','Cameras')] } else 
                  {select(desc_camera[,-10],bairro,!!input[["cameras-filters-Órgão"]]) }  ,
                  by=c('bairro'='bairro') )
        
      }
      
      if('Torre' %in% input$segurança )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = Torre,
                           group='id_torre',
                           popup = ~Name,
                           weight = 1,
                           radius =6,
                           fillColor = '#870BDE' ,
                           color='#FA3633',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")
          ) |>
          addLegend("bottomright",colors =  '#870BDE',
                    labels =c('Célula de proteção'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_torre ,by=c('bairro'='bairro'))
        
        
      }
      
      if('Territorios_PMPU' %in% input$segurança )
      {
        leafletProxy("mapa_base")  |>
          addPolygons(data = Territorios_PMPU,
                           group='id_Territorios_PMPU',
                           popup = ~Name,
                           fillColor = '#9880F5' ,
                           color='#6A21FA',
                           weight=2,
                           stroke = T, fillOpacity = .6 ,opacity = 1,
                           options = pathOptions(pane = "poligono")
          ) |>
          addLegend("bottomright",colors =  '#9880F5',
                    title = 'Território',
                    labels =c('Célula de proteção comunitária'),
                    opacity = 1)
        descrit_pop <- left_join(descrit_pop,desc_pmpu ,by=c('bairro'='bairro'))
        
      }
      
      if('Territorios_GEMP' %in% input$segurança )
      {
        leafletProxy("mapa_base")  |>
          addPolygons(data = Territorios_GEMP,
                      group='id_Territorios_GEMP',
                      popup = ~Name,
                      fillColor = '#DE3EBF' ,
                      color='#CC2572',
                      weight=2,
                      stroke = T, fillOpacity = .6 ,opacity = 1,
                      options = pathOptions(pane = "poligono")
          ) |>
          addLegend("bottomright",colors =  '#DE3EBF',
                    title = 'Território',
                    labels =c('Grupamento Especializado Maria da Penha'),
                    opacity = 1)
        
        
      }
      
      
      if('Areninhas' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = st_as_sf(AreninhasR()),
                           group='id_Areninhas',
                           popup = popupTable(AreninhasR() ,
                                              zcol = c("nome", "data_inaug", "tipo"),
                                              feature.id = FALSE,
                                              row.numbers = FALSE) ,
                           weight = 1,
                           radius =4,
                           fillColor =colAreninhas(AreninhasR()$tipo)  , 
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")
                           ) |>
          addLegend("bottomright", pal = colAreninhas,
                    values = AreninhasR()$tipo,
                    #title = "Tipo",
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,
                                 if(is.null(input[["Areninhas-filters-tipo"]]))
                                 {desc_areninha } else 
                                 {select(desc_areninha,bairro,!!input[["Areninhas-filters-tipo"]]) }  ,
                                 by=c('bairro'='bairro') )
        
      }
      
      if('AdocaoPracasEAreasVerdes' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = AdocaoPracasEAreasVerdes,
                           group='id_AdocaoPracasEAreasVerdes',
                           popup = popupTable(AdocaoPracasEAreasVerdes,
                                              zcol = c("Nome", "Adotante", "ano_ref"),
                                              feature.id = FALSE,
                                              row.numbers = FALSE) ,
                           weight = 1,
                           radius =4,
                           fillColor = '#6EBD40' ,
                           color='#62246B',
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")
                           ) |>
          addLegend("bottomright",colors =  '#6EBD40',
                    labels =c('Adoção Pracas e Áreas Verdes'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_adocao,by=c('bairro'='bairro') )
        
      }
      
      if('Zeis' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addPolygons(data = st_as_sf(ZeisR()),
                      group='id_Zeis',
                      color=colZeis(ZeisR()$Zeis) , weight = 6,
                      fillColor = colZeis(ZeisR()$Zeis) ,
                      stroke = T, fillOpacity = 1 ,opacity = 1  ) |>
          addLegend("bottomright",pal = colZeis,
                    values = ZeisR()$Zeis,
                    title = 'Zeis',
                    opacity = 1)
      }
      
      if('Pracas' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addPolygons(data = Pracas,
                      group='id_Pracas',
                      popup = popupTable(Pracas ,
                                         zcol = c("nome",'nome_pop','area m2'),
                                         feature.id = FALSE,
                                         row.numbers = FALSE) ,
                      color= '#464FEB',
                      fillColor = '#3578D4' ,
                      weight=2,
                      stroke = T, fillOpacity = .7 ,opacity = 1,
                      options = pathOptions(pane = "poligono")
                      ) |>
          addLegend("bottomright",colors =  '#3578D4',
                    labels =c('Praças'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_praca,by=c('bairro'='bairro') )
      }
      
      if('Pracas_Vivas' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = Pracas_Vivas ,
                           group='id_Pracas_Vivas',
                           popup = popupTable(Pracas_Vivas ,
                                              zcol = c("ano_criaca"),
                                              feature.id = FALSE,
                                              row.numbers = FALSE) ,
                           weight = 1,
                           radius =4,
                           color='#62246B',
                           fillColor ='#EB1EEA'  , 
                           stroke = T, fillOpacity = 1,
                           options = pathOptions(pane = "pontos")
                           ) |>
          addLegend("bottomright",colors =  '#EB1EEA',
                    labels =c('Praças Vivas'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_pracaviva,by=c('bairro'='bairro') )
        
      }
      
      if('cultura' %in% input$infraestrutura )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = st_as_sf(culturaR()) ,
                           group='id_cultural',
                           popup = popupTable(culturaR() ,
                                              zcol = c("nome",'tipo'),
                                              feature.id = FALSE,
                                              row.numbers = FALSE) ,
                           weight = 1,
                           radius =6,
                           color='black',
                           fillColor =colcultura(culturaR()$tipo)  , 
                           stroke = T, fillOpacity = 1  ) |>
          addLegend("bottomright",pal = colcultura,
                    values = culturaR()$tipo,
                    title = 'Equipamentos de cultura',
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_cult,by=c('bairro'='bairro') )
        
        
      }
      
      if('eco' %in% input$socioeconomica )
      {
        leafletProxy("mapa_base")  |>
          addCircleMarkers(data = eco,
                      group='id_eco',
                      color= 'green',
                      fillColor = '#0DA16D' ,
                      weight=2,
                      popup=popupTable(eco ,
                                       zcol = c("nome",'modalidade','data_inaug'),
                                       feature.id = FALSE,
                                       row.numbers = FALSE),
                      stroke = T, fillOpacity = 1 ,opacity = 1,
                      options = pathOptions(pane = "pontos")
                      ) |>
          addLegend("bottomright",colors =  '#0DA16D',
                    labels =c('Ecopontos'),
                    opacity = 1)
        
        descrit_pop <- left_join(descrit_pop,desc_eco , by=c('bairro'='bairro'))
        
        
      }
      
      leafletProxy("mapa_base")  |>
        addPolygons(data=shp_Bairros,
                    weight=1,
                    opacity = 0.5,
                    fillColor = "white",
                    popup =  popupTable(left_join(descrit, descrit_pop,by=c('bairro'='bairro')),
                                        feature.id = FALSE,
                                        row.numbers = FALSE),
                    dashArray = "0",
                    fillOpacity = 0,
                    group = "Bairros",
                   options = pathOptions(pane = "bairros")
                    )
      
      
    })
    
     

  })
}
