## load necessary r files
source ("data/process.R")
source ("ui.R")
source ("server.R")

## run app
shinyApp(ui = ui, server = server)
