#Set working directory
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")

#Load libraries
library(shiny)
library(ggplot2)
library(plotly)

#Assemble page
hist_page_content<-sidebarLayout(
  sidebarPanel(
    selectInput(
      inputId = "x_var_hist",
      label = "x variable",
      choices = list(
        "falling velocity (cm/s)" = "mean_velocity.md",
        "surface roughness" = "mean_rough",
        "height (cm)" = "height",
        "likely wind dispersal range (m)" = "mean_long")
    ),
  ),
  mainPanel(
    plotOutput("hist")
  ))

hist_panel<-tabPanel(
  "Histogram",
  hist_page_content,
  p("Average values for a each species are counted.")
)
