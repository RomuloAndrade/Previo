# setwd("C:/Dados/Dash/taxa-natalidade-bairros-main")
# renv::activate()

.rs.restartR()

renv::refresh()

shiny::shinyAppDir(
  ".",
  options = list(
    port = 7171,
    launch.browser = TRUE
  )
)


#renv::install('readxl')

#y

#https://stackoverflow.com/questions/62642615/adding-reactive-popup-graphs-plots-to-a-leaflet-map-with-shiny-r
