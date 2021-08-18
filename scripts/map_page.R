# GBIF data citation:
# GBIF.org (31 July 2021) GBIF Occurrence Download https://doi.org/10.15468/dl.74fcn4

#Set working directory
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")

#Load libraries
library(shiny)
library(leaflet)
#Assemble page
map_page_content<- sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "point_size",
      label = "point size",
      choices = list(
        "falling velocity",
        "surface roughness",
        "height",
        "likely wind dispersal range")
      ),
    ),
  mainPanel(
    leafletOutput("map")))

map_panel<-tabPanel(
  "Mapped Data",
  map_page_content,
  p("Occurence data collected from GBIF.org (31 July 2021) GBIF Occurrence 
    Download https://doi.org/10.15468/dl.74fcn4. Occurences limited to Brazil, 
    Venezuela, and Tanzania to highlight regions of interest. Points do not 
    represent collected specimens shown in other pages of this app.")
)

