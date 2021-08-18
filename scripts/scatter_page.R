#Set working directory
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")

#Load libraries
library(shiny)
library(ggplot2)
library(plotly)

#Assemble page
scatter_page_content<-sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "x_var_scatter",
      label = "x variable",
      choices = list(
        "falling velocity (cm/s)" = "mean_velocity.md",
        "surface roughness" = "mean_rough",
        "height (cm)" = "height",
        "likely wind dispersal range (m)" = "mean_long")
    ),
    selectInput(
      inputId = "y_var_scatter",
      label = "y variable",
      choices = list(
        "falling velocity (cm/s)" = "mean_velocity.md",
        "surface roughness" = "mean_rough",
        "height (cm)" = "height",
        "likely wind dispersal range (m)" = "mean_long")
    ),
  ),
  mainPanel(
    plotlyOutput("scatter")
  ))

scatter_panel<-tabPanel(
  "Scatter Plot",
  scatter_page_content,
  p("Each point represents average value for a given species.")
)
