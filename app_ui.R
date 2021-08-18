library(shiny)
library(plotly)
library(leaflet)
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")
source("scripts/intro_page.R")
source("scripts/map_page.R")
source("scripts/scatter_page.R")
source("scripts/bar_page.R")
source("scripts/hist_page.R")


#Assemble ui
ui <- fluidPage(
  navbarPage(
  "Variation in Grass Seed Dispersal in Tropical Savannas",
  intro_panel,
  bar_panel,
  scatter_panel,
  hist_panel,
  map_panel
  )
)
