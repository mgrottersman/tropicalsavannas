#Set working directory
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")

#Load libraries
library(shiny)
library(ggplot2)
library(plotly)

#Assemble page
bar_page_content<-sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "x_var_bar",
      label = "x variable",
      choices = list(
        "falling velocity (cm/s)",
        "surface roughness",
        "height (cm)",
        "likely wind dispersal range (m)")
    ),
  ),
  mainPanel(
    plotlyOutput("bar")
  ))

bar_panel<-tabPanel(
  "Bar Chart",
  bar_page_content,
  p("Chart shows average values normalized to species by region.")
)
