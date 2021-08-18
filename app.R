setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")
library("shiny")

# sourcing ui and server
source("app_ui.R")
source("app_server.R")

# build the app
shinyApp(ui = ui, server = server)
