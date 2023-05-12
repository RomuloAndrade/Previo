setwd("C:/Dados/Dash/taxa-natalidade-bairros-main")

library(tidyverse)
library(sf)
library(leaflet)
# shp_Bairros <- readRDS('app/data/shp_Bairros/shp_Bairros.rds') |>
#   select(-bairro_rais,-cod_bairro) |>
#   mutate(areaB = st_area(geometry) %>% as.numeric())
# 
# write_sf(shp_Bairros,'app/data/shp_Bairros/shp_BairrosA.shp')
shp_Bairros <- st_read('app/data/shp_Bairros/shp_BairrosA.shp')


bairros <- shp_Bairros |> 
  st_drop_geometry() |> 
  select(bairro)
#write.csv(Descritiva,'app/data/nomes_bairros.csv',row.names = F)

Descritiva <- read_csv('app/data/Descritiva/Descritiva.csv',show_col_types = FALSE) |>
  select(bairro,Polig_atuac)



nome_bairro <- read_csv('app/data/nomes_bairros.csv')
## ARENINHAS

Areninhas <- read_sf('app/data/Areninhas/vw_Areninhas.shp',options = "ENCODING=WINDOWS-1252") |>
  group_by(tipo)


data_split<- split(Areninhas, Areninhas$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res)

write.csv(descrit,'app/data/Areninhas/desc_areninha.csv',row.names = F)
read.csv('app/data/Areninhas/desc_areninha.csv',check.names=F)


## Praças vivas

Pracas_Vivas<- read_sf('app/data/Pracas/Praças_Vivas/vw_PracasVivas.shp',
                       options = "ENCODING=WINDOWS-1252")

desc_pracaviva <-nome_bairro |> mutate(`Praças vivas`= lengths(st_intersects(shp_Bairros,Pracas_Vivas)))
write.csv(desc_pracaviva,'app/data/Pracas/Praças_Vivas/desc_pracaviva.csv',row.names = F)

## Praça 

Pracas<- read_sf('app/data/Pracas/Praças_de_Fortaleza/vw_PracasFortaleza.shp',
                       options = "ENCODING=WINDOWS-1252",promote_to_multi = T) 
Pracas |> 
  st_drop_geometry() |> 
  mutate(id=as.character(id)) |> 
summarise(cid=n())

distinct()

desc_praca <-nome_bairro |> mutate(Praças= lengths(st_intersects(shp_Bairros,Pracas[,1]) ))
sum(desc_praca$Praças)
write.csv(desc_praca,'app/data/Pracas/Praças_de_Fortaleza/desc_praca.csv',row.names = F)

## AdocaoPracasEAreasVerdes
AdocaoPracasEAreasVerdes <- read_sf('app/data/Pracas/Adoção_de_Praças_e_Áreas_Verdes/vw_AdocaoPracasEAreasVerdes.shp',
options = "ENCODING=WINDOWS-1252",quiet =T)

desc_adocao <-nome_bairro |> mutate('Adoção áreas verdes'= lengths(st_intersects(shp_Bairros,AdocaoPracasEAreasVerdes)))

write.csv(desc_adocao,'app/data/Pracas/Adoção_de_Praças_e_Áreas_Verdes/desc_adocao.csv',row.names = F)
sum(desc_adocao$'Adoção áreas verdes')

## Eco

eco <- read_sf('app/data/vw_Ecopontos/vw_Ecopontos.shp',
                                    options = "ENCODING=WINDOWS-1252",quiet =T)

desc_eco <-nome_bairro |> mutate('Ecopontos'= lengths(st_intersects(shp_Bairros,eco)) |> as.numeric())
write.csv(desc_eco,'app/data/vw_Ecopontos/desc_eco.csv',row.names = F)


## Conselhos_Tutelares

Conselhos_Tutelares <- st_read('app/data/vw_ConselhosTutelares/vw_ConselhosTutelares.shp',
                               options = "ENCODING=WINDOWS-1252") |> 
  st_cast("POINT")

desc_cons_tut <-nome_bairro |> mutate('Conselhos Tutelares'= lengths(st_intersects(shp_Bairros,Conselhos_Tutelares)) |> as.numeric())
write.csv(desc_cons_tut,'vw_ConselhosTutelares/desc_cons_tut.csv',row.names = F)



## Equip Assist Social

Equip_Assist_Social <- st_read('app/data/vw_EquipamentosAssistenciaSocial/vw_EquipamentosAssistenciaSocial.shp',
                               options = "ENCODING=WINDOWS-1252",quiet =T)  |>  
  select(nome,tipo,fone)  |>  
  st_cast("POINT") |> 
  mutate(tipo=case_when(
    tipo=="Unidade de Acolhimento Pousada Social" ~ "Unidade de Acolhimento",
    tipo=="Unidade de Acolhimento Institucional para Crianças e Adolescentes" ~ "Unidade de Acolhimento",
    tipo=="Unidade de Acolhimento Pousada Social"~'Pousada Social',
    TRUE ~ tipo  
  )) |> 
  group_by(tipo)


data_split<- split(Equip_Assist_Social, Equip_Assist_Social$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res)

write.csv(descrit,'app/data/vw_EquipamentosAssistenciaSocial/desc_social.csv',row.names = F)


# Samu

Samu  <- st_read('app/data/vw_BasesSAMU/vw_BasesSAMU.shp',options = "ENCODING=WINDOWS-1252",quiet =T) 

desc_samu <-nome_bairro |> mutate('Base samu'= lengths(st_intersects(shp_Bairros,Samu)) |> as.numeric())
write.csv(desc_samu,'app/data/vw_BasesSAMU/desc_samu.csv',row.names = F)

desc_samu <- read_csv('app/data/vw_BasesSAMU/desc_samu.csv',show_col_types = FALSE)[,-2]

# Saude

EquipamentosSaude_cnes <- read_sf('app/data/202303_EstabelecimentosSaudeCNES/saude.shp',quiet =T)

data_split<- split(EquipamentosSaude_cnes, EquipamentosSaude_cnes$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res)

write.csv(descrit,'app/data/202303_EstabelecimentosSaudeCNES/desc_saude.csv',row.names = F)
read_csv('app/data/202303_EstabelecimentosSaudeCNES/desc_saude.csv',show_col_types = FALSE)[,-2]

# escola

Escolas_munic <-  read_sf('app/data/Escolas/escolas.shp') 

data_split<- split(Escolas_munic, Escolas_munic$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res)

write.csv(descrit,'app/data/Escolas/desc_escola.csv',row.names = F)
read_csv('app/data/Escolas/desc_escola.csv',show_col_types = FALSE)[,-2]

#### Cultura

Equip_cult <- st_read('app/data/tblCultura/Equip_cult.shp',quiet =T) |> 
  st_cast("POINT") |> 
  mutate(tipo=case_when(tipo=='Biblioteca Pública'~'Biblioteca',
                        tipo=='Centros Urbanos de Cultura, Arte, Ciência e Esporte'~'CUCA',
                        TRUE ~tipo))


data_split<- split(Equip_cult, Equip_cult$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res)

write.csv(descrit,'app/data/tblCultura/desc_cultura.csv',row.names = F)
read_csv('app/data/tblCultura/desc_cultura.csv',show_col_types = FALSE)[,-2]

##### Aglomerado

Aglomerado <- read_sf('app/data/assentamento_ibge/Aglomerado.shp')  

pi <- st_intersection(Aglomerado ,shp_Bairros )

attArea <- pi %>% 
  mutate(areafim = st_area(.) %>% as.numeric())

attArea_group <- attArea %>% 
  as_tibble() %>% 
  group_by(bairro,areaB) %>% 
  summarize(area_aglom = sum(areafim))

descrit <- left_join(nome_bairro, attArea_group) |> 
  mutate('Área aglomerado (%)'= round(100*area_aglom/areaB ,2 ))

write.csv(descrit,'app/data/assentamento_ibge/desc_aglomerado.csv',row.names = F)
read_csv('app/data/assentamento_ibge/desc_aglomerado.csv',show_col_types = FALSE)[,-2]


# Mediação
Mediacao <- st_read('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/Mediacao_conflitos.shp',quiet =T) 

desc_mediacao <-nome_bairro |> mutate('Mediação conflitos'= lengths(st_intersects(shp_Bairros,Mediacao)) |> as.numeric())
write.csv(desc_mediacao,'app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/desc_mediacao.csv',row.names = F)

desc_mediacao <- read_csv('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/desc_mediacao.csv',show_col_types = FALSE)[,-2]


# BasesPMPU
Torre <-  read_sf('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/Bases_PMPU.shp',quiet =T)

desc_torre <-nome_bairro |> mutate('Célula de proteção'= lengths(st_intersects(shp_Bairros,Torre)) |> as.numeric())
write.csv(desc_torre,'app/data/Sesec/202304_BasesPMPU2022SESECNOVO/desc_torre.csv',row.names = F)

desc_torre <- read_csv('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/desc_torre.csv',show_col_types = FALSE)[,-2]

# Cameras
Cameras <- st_read('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_geral.shp',quiet =T) 


data_split<- split(Cameras, Cameras$Órgão)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(nome_bairro,res,Cameras=rowSums(desc_camera[,-1]))

write.csv(descrit,'app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/desc_camera.csv',row.names = F)
desc_camera <-read_csv('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/desc_camera.csv',show_col_types = FALSE)[,-2]

### Atuação celulas

Territorios_PMPU <- read_sf('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/Territorios_PMPU.shp')

pi <- st_intersection(Territorios_PMPU ,shp_Bairros )

attArea <- pi %>% 
  mutate(areaPol= st_area(.) %>% as.numeric())

attArea_group <- attArea %>% 
  as_tibble() %>% 
  group_by(bairro,areaB) %>% 
  summarize(areaPol = sum(areaPol))

descrit <- left_join(nome_bairro, attArea_group) |> 
  mutate('Área território (%)'= round(100*areaPol/areaB ,2 )) #|> 
  #select(1,2,5)

write.csv(descrit,'app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/desc_pmpu.csv',row.names = F)
desc_pmpu <- read_csv('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/desc_pmpu.csv',show_col_types = FALSE)











# PROCESSAMENTO DADOS FINAIS
########################################
#PROCESSAMENTO DADOS FINAIS
####
library(tidyverse)
setwd("C:/Dados/previo/Projeto_bairros_indicadores")
iluminacao <- readxl::read_xlsx('Iluminação/Resumo do Parque de Iluminação 01-23.xlsx',skip=1,sheet= ' % Bairro') |> 
  drop_na() |> 
  select(2,3) |> 
  mutate(Bairro_I=Bairro)

shp_Bairros <- shp_Bairros |> 
  mutate(Bairro_S=str_to_upper(bairro),
         Bairro_S = stringi::stri_trans_general(str = Bairro_S, id = "Latin-ASCII")) 


iluminacao_shp <- left_join(shp_Bairros,iluminacao,by=c('Bairro_S'='Bairro_I'))
####

df <- df %>%
  dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                lat = sf::st_coordinates(.)[,2])
####

### SESEC
setwd("C:/Dados/Dash/taxa-natalidade-bairros-main")
library(sf)
library(leaflet)
library(tidyverse)
# Bases_PMPU

Bases_PMPU  <- st_read('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/Bases_PMPU_2022_SESEC.shp') |>
  st_transform(4326) |> 
  st_cast("POINT") |> 
  dplyr::select(Name,descriptio)

write_sf(Bases_PMPU,'app/data/Sesec/202304_BasesPMPU2022SESECNOVO/Bases_PMPU.shp')
sx <- read_sf('app/data/Sesec/202304_BasesPMPU2022SESECNOVO/Bases_PMPU.shp')  

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=sx)

#NucleosMediacaoConflitos
Mediacao  <- st_read('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/Nucleos_COMED_SESEC.shp') |>
  st_transform(4326) |> 
  dplyr::select(Nome_Abrev,Latitude,Longitude)

write_sf(Mediacao,'app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/Mediacao_conflitos.shp')
sx <- read_sf('app/data/Sesec/202304_NucleosMediacaoConflitosSESECNOVO/Mediacao_conflitos.shp')  

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=sx)



#cameras
cameras_PMPU  <- st_read('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/Posteamento_cameras_PMPU_SESEC.shp') |>
  st_transform(4326) |> 
  st_zm(drop = TRUE, what = "ZM") |> 
  st_cast("POINT") |>
  mutate(Órgão='TORRES_GMF') |> 
  dplyr::select(Órgão) 



write_sf(cameras_PMPU,'app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_PMPU.shp')
sx <- read_sf('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_PMPU.shp')  

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=sx)

Cameras <- st_read('app/data/20221201_CamerasVideomonitoramentoAMC/20221201_CamerasVideomonitoramentoAMC.shp',quiet =T) 

Cameras_mod <- Cameras |> 
  select(Órgão) |> 
  filter(Órgão!="TORRES_GMF")



Cameras_geral <- rbind(Cameras_mod,cameras_PMPU) 

write_sf(Cameras_geral,'app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_geral.shp')
sx <- read_sf('app/data/Sesec/202304_PosteamentoCamerasPMPUSESECNOVO/cameras_geral.shp')  

# Territorio PMPU

TerritoriosPMPU  <- st_read('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/Territorios_PMPU_2022_SESEC.shp') |>
  st_transform(4326) |>  
  dplyr::select(Name) 



write_sf(TerritoriosPMPU,'app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/Territorios_PMPU.shp')
sx <- read_sf('app/data/Sesec/202304_TerritoriosPMPU2022SESECNOVO/Territorios_PMPU.shp')  

leaflet() |> 
  addTiles() |> 
  addPolygons(data=sx)

#MicroterritoriosGEMPGMF
TerritoriosGEMP  <- st_read('app/data/Sesec/202304MicroterritoriosGEMPGMFNOVO/Microterritorios_GEMP_GMF.shp') |>
  st_transform(4326) |>  
  dplyr::select(Name) 



write_sf(TerritoriosGEMP,'app/data/Sesec/202304MicroterritoriosGEMPGMFNOVO/TerritoriosGEMP.shp')
sx <- read_sf('app/data/Sesec/202304MicroterritoriosGEMPGMFNOVO/TerritoriosGEMP.shp')  

leaflet() |> 
  addTiles() |> 
  addPolygons(data=sx,fillColor = 'red') |> 
  addPolygons(data=TerritoriosPMPU,fillColor = 'blue')


# SAUDE
#Estabelecimento de saúde
saude  <- st_read('app/data/202303_EstabelecimentosSaudeCNES/202303_EstabelecimentosSaudeCNES.shp') |>
  st_transform(4326) |>  
  filter(DS_TIPO_UN %in% 
           c("CENTRO DE SAUDE/UNIDADE BASICA",
             "PRONTO ATENDIMENTO",
             "HOSPITAL GERAL",
             "HOSPITAL ESPECIALIZADO",
             "CENTRO DE ATENCAO PSICOSSOCIAL")) |>       
  dplyr::select(NO_FANTASI,NU_LATITUD,NU_LONGITU,DS_TIPO_UN) |> 
  rename(nome=NO_FANTASI,
         tipo=DS_TIPO_UN ) |> 
  mutate(tipo=case_when(
    tipo=="CENTRO DE SAUDE/UNIDADE BASICA"~'UAPS',
    tipo=="PRONTO ATENDIMENTO"~'UPA',
    tipo=="CENTRO DE ATENCAO PSICOSSOCIAL"~ 'CAPS',
    TRUE~tipo
  ))



write_sf(saude,'app/data/202303_EstabelecimentosSaudeCNES/saude.shp')
saude <- read_sf('app/data/202303_EstabelecimentosSaudeCNES/saude.shp')  

leaflet() |> 
  addTiles() |> 
  addCircleMarkers(data=saude,fillColor = 'red') 

# ESCOLAS MUNICIPAIS SME
Escolas_munic <-  read_csv2('app/data/Escolas/202304_UnidadesEnsinoSMEGeolocalizadas.csv') |> 
  select(TIPO,`UNIDADE ESCOLAR`,lat,lon) |> 
  st_as_sf(coords = c("lon","lat"),crs=4326)

leaflet(Escolas_munic) |> 
  #addTiles() |> 
  addCircleMarkers() 
# leaflet() |> 
#   #addTiles() |> 
#   addPolygons(data=Aglomerado) 


# Aglomerados subnormais ibge


Aglomerado <- st_read('app/data/assentamento_ibge/aglomerados_fortaleza.shp',quiet =T) |>
  st_transform(4326) |>
  mutate(area = st_area(geometry) |>  as.numeric()) |>
  rename(nome=NM_AGSN)

write_sf(Aglomerado,'app/data/assentamento_ibge/Aglomerado.shp')






area_aglom = aggregate(Aglo_point, shp_Bairros_sfc  , sum,join =st_intersects) |> 
  st_drop_geometry() |> 
  cbind(nome_bairro)

class(shp_Bairros)
 descrit <- st_intersects(shp_Bairros,Aglomerado)
# 
res <- lapply(data_split, function(x)
  lengths(st_intersects(shp_Bairros,x)))|>
  as_tibble( )





# Aglomerado_area <- Aglomerado %>% 
#   mutate(area_Aglo = st_area(.) %>% as.numeric())
# 
# shp_Bairros_area<- shp_Bairros %>% 
#   mutate(area_Bairro = st_area(.) %>% as.numeric())
a <- st_join(shp_Bairros, Aglomerado)  |>  
  group_by(bairro) |> 
  summarise(sum(Aglomerado_area))



Inter <- st_intersection(shp_Bairros,Aglomerado)
Inter |> arrange(bairro)

Inter_area<- Inter %>%
  mutate(area_Inter = st_area(.)  %>% as.numeric()) %>%
  st_drop_geometry()


Aglor_area_Bairro <- Inter_area |>
  group_by(bairro) |>  
  summarize(area_Inter = sum(area_Inter))

areas<- left_join(shp_Bairros_area |> st_drop_geometry(),Aglor_area_Bairro)

areasF <- areas %>% mutate(Perc_assent=round((area_Inter/area_Bairro)*100,2))
areasF

plot(shp_Bairros$geometry, axes = TRUE)
plot(Aglomerado$geometry, add = TRUE,col = 'blue')
plot(Inter$geometry, add = TRUE,col = 'red')


# for each field, get area per Assentamento
Area_assent <- attArea %>% 
  as_tibble() %>% 
  group_by(Poligono) %>% 
  summarize(area_ = sum(area_))

# for each field, shp_atuacao

shp_atuacao  <- shp_atuacao %>% 
  mutate(area_Poligono = st_area(.) %>% as.numeric()) 

areas<- left_join(Area_assent,shp_atuacao)

areas <- areas %>% mutate(Perc_assent=(area_/area_Poligono)*100)
attArea


m1 = cbind(c(0, 0, 1, 0), c(0, 1, 1, 0))
m2 = cbind(c(0, 1, 1, 0), c(0, 0, 1, 0))
pol = st_sfc(st_polygon(list(m1)), st_polygon(list(m2)))
set.seed(1985)
d = data.frame(matrix(runif(15), ncol = 3))
p = st_as_sf(x = d, coords = 1:2)
descrit <- st_intersects(pol,p)
descrit |> aggregate(list(.$a.x), mean)
plot(pol)
plot(p, add = TRUE)

res <- lapply(data_split, function(x)
  lengths(st_intersects(shp_Bairros,x)))|>
  as_tibble( )
class(pol)


#> Simple feature collection with 2 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 0 ymin: 0 xmax: 1 ymax: 1
#> CRS:           NA
#>          X3                       geometry
#> 1 0.5951766 POLYGON ((0 0, 0 1, 1 1, 0 0))
#> 2 0.2997908 POLYGON ((0 0, 1 0, 1 1, 0 0))



library(sf)
library(tidyverse)

# example data from raster package
soil <- st_read(system.file("external/lux.shp", package="raster")) %>% 
  # add in some fake soil type data
  mutate(soil = LETTERS[c(1:6,1:6)]) %>% 
  select(soil)

# field polygons
field <- c("POLYGON((6 49.75,6 50,6.4 50,6.4 49.75,6 49.75))",
           "POLYGON((5.8 49.5,5.8 49.7,6.2 49.7,6.2 49.5,5.8 49.5))") %>% 
  st_as_sfc(crs = st_crs(soil)) %>% 
  st_sf(field = c('x','y'), geoms = ., stringsAsFactors = FALSE)

# intersect - note that sf is intelligent with attribute data!
pi <- st_intersection(Aglomerado ,shp_Bairros )


# add in areas in m2
attArea <- pi %>% 
  mutate(areafim = st_area(.) %>% as.numeric())

# for each field, get area per soil type
datatest <- attArea %>% 
  as_tibble() %>% 
  group_by(bairro, nome) %>% 
  summarize(areasum = sum(areafim))


        