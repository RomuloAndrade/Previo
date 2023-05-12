Areninhas <- read_sf('app/data/Areninhas/vw_Areninhas.shp',options = "ENCODING=WINDOWS-1252") |>
  group_by(tipo)

 
data_split<- split(Areninhas, Areninhas$tipo)

res <- lapply(data_split, function(x) 
  lengths(st_intersects(shp_Bairros,x)))|> 
  as_tibble( )

descrit <- cbind(shp_Bairros |> 
                   st_drop_geometry() |> 
                   select(bairro),res)

write.csv(descrit,'app/data/Areninhas/desc_areninha.csv',row.names = F)
read.csv('app/data/Areninhas/desc_areninha.csv',check.names=F)

bairros <- shp_Bairros |> 
  st_drop_geometry() |> 
  select(bairro)
write.csv(bairros,'app/data/nomes_bairros.csv',row.names = F)

