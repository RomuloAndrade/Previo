box::use(
  shiny,
  thematic,
  app/ui[ui],
  app/server[server]
)

thematic$thematic_shiny()

shiny$shinyApp(ui, server)
#y

#renv::install('sf')
#.rs.restartR()
#renv::status()
# N
#N
# y
