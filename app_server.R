#Working Directory and sources
setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")
source("scripts/map_page.R")
source("scripts/scatter_page.R")
source("scripts/bar_page.R")
source("scripts/hist_page.R")

#Load libraries
library(dplyr)
library(shiny)
library(leaflet)
library(plotly)

#Import data
gbif<-read.delim('data/210731_GBIF.csv')
master_data<-read.csv('data/210731_master_data')

#Clean and combine data
gbif<-gbif %>% 
  filter(countryCode==c('TZ','BR','VE'))
gbif<-subset(gbif, select=c(species, decimalLatitude, decimalLongitude))

master_by_species<-master_data %>% 
  group_by(species) %>% 
  summarize(mean_velocity.md=mean(velocity.md,na.rm=TRUE),
            mean_rough=mean(rough,na.rm=TRUE),
            mean_long=mean(long,na.rm=TRUE),
            mean_med=mean(med,na.rm=TRUE),
            region=region,
            height=height)%>% 
  unique()

gbif_master<-merge(gbif, master_by_species, by='species')
gbif_master$mean_roughx50<-50*(gbif_master$mean_rough)
gbif_master$mean_velocity.mdx.25<-0.25*(gbif_master$mean_velocity.md)
gbif_master$heightx.25<-0.25*(gbif_master$height)

standard_error <- function(x) sd(x)/sqrt(length(x))

master_by_region<-master_by_species %>% 
  group_by(region) %>% 
  summarize(velocity_mean=mean(mean_velocity.md,na.rm=TRUE),
            velocity_se=standard_error(na.omit(mean_velocity.md)),
            rough_mean=mean(mean_rough,na.rm=TRUE),
            rough_se=standard_error(na.omit(mean_rough)),
            long_mean=mean(mean_long,na.rm=TRUE),
            long_se=standard_error(na.omit(mean_long)),
            height_mean=mean(height),
            height_se=standard_error(na.omit(height))) %>% 
  unique()

velocity_paleo_vector<-master_by_species %>% 
  filter(region=='Paleotropical') %>% 
  pull(mean_velocity.md)
velocity_neo_vector<-master_by_species %>% 
  filter(region=='Neotropical') %>% 
  pull(mean_velocity.md)
rough_paleo_vector<-master_by_species %>% 
  filter(region=='Paleotropical') %>% 
  pull(mean_rough)
rough_neo_vector<-master_by_species %>% 
  filter(region=='Neotropical') %>% 
  pull(mean_rough)
long_paleo_vector<-master_by_species %>% 
  filter(region=='Paleotropical') %>% 
  pull(mean_long)
long_neo_vector<-master_by_species %>% 
  filter(region=='Neotropical') %>% 
  pull(mean_long)
height_paleo_vector<-master_by_species %>% 
  filter(region=='Paleotropical') %>% 
  pull(height)
height_neo_vector<-master_by_species %>% 
  filter(region=='Neotropical') %>% 
  pull(height)

master_by_species_neo<-master_by_species[master_by_species$region=='Neotropical',]
master_by_species_paleo<-master_by_species[master_by_species$region=='Paleotropical',]

pal <- colorFactor(c("palegreen3", "greenyellow"), domain = c("Neotropical", "Paleotropical"))

#Server
server <- function(input, output) {
  
  #Bar
  output$bar<-renderPlotly(
    if(input$x_var_bar=="falling velocity (cm/s)"){
      ggplotly(ggplot(data = master_by_region, mapping = aes(x = c('neotropical','paleotropical'),y=velocity_mean, fill=c('neotropical','paleotropical'))) +
              geom_bar(stat = "identity", color = "black") +
              scale_fill_manual(values = c('palegreen3', 'greenyellow'), guide = FALSE)+
              geom_errorbar(aes(ymin =(velocity_mean-velocity_se), ymax =(velocity_mean+velocity_se)), width =0.1, colour='black', alpha=0.9, size=1.0) + 
              scale_x_discrete(labels=c("neotropical", "paleotropical"))+
                theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
              tooltip = "y") %>% 
        layout(annotations=
                 list(x = 1, y = 0,
                      showarrow=F,
                      xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', 
                      xshift=0, yshift=0,
                      text=paste0('p=',
                                  t.test(velocity_neo_vector,velocity_paleo_vector)$p.value)),
               font=list(size=8))
      }
    else if(input$x_var_bar=="surface roughness"){
      ggplotly(ggplot(data = master_by_region, mapping = aes(x = c('neotropical','paleotropical'),y=rough_mean, fill=c('neotropical','paleotropical'))) +
              geom_bar(stat = "identity", color = "black") +
              scale_fill_manual(values = c('palegreen3', 'greenyellow'), guide = FALSE)+
              geom_errorbar(aes(ymin =(rough_mean-rough_se), ymax =(rough_mean+rough_se)), width =0.1, colour='black', alpha=0.9, size=1.0) + 
              scale_x_discrete(labels=c("neotropical", "paleotropical"))+
                theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
              tooltip = "y")%>% 
        layout(annotations=
                 list(x = 1, y = 0,
                      showarrow=F,
                      xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', 
                      xshift=0, yshift=0,
                      text=paste0('p=',
                                  t.test(rough_neo_vector,rough_paleo_vector)$p.value)),
               font=list(size=8))
    }
    else if(input$x_var_bar=="height (cm)"){
     ggplotly(ggplot(data = master_by_region, mapping = aes(x = c('neotropical','paleotropical'),y=height_mean, fill=c('neotropical','paleotropical'))) +
              geom_bar(stat = "identity", color = "black") +
              scale_fill_manual(values = c('palegreen3', 'greenyellow'), guide = FALSE)+
              geom_errorbar(aes(ymin =(height_mean-height_se), ymax =(height_mean+height_se)), width =0.1, colour='black', alpha=0.9, size=1.0) + 
              scale_x_discrete(labels=c("neotropical", "paleotropical"))+
                theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
              tooltip = "y")%>% 
        layout(annotations=
                 list(x = 1, y = 0,
                      showarrow=F,
                      xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', 
                      xshift=0, yshift=0,
                      text=paste0('p=',
                                  t.test(height_neo_vector,height_paleo_vector)$p.value)),
               font=list(size=8))
    }
    else if(input$x_var_bar=="likely wind dispersal range (m)"){
      ggplotly(ggplot(data = master_by_region, mapping = aes(x = c('neotropical','paleotropical'),y=long_mean, fill=c('neotropical','paleotropical'))) +
              geom_bar(stat = "identity", color = "black") +
              scale_fill_manual(values = c('palegreen3', 'greenyellow'), guide = FALSE)+
              geom_errorbar(aes(ymin =(long_mean-long_se), ymax =(long_mean+long_se)), width =0.1, colour='black', alpha=0.9, size=1.0) + 
              scale_x_discrete(labels=c("neotropical", "paleotropical"))+
                theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
              tooltip = "y")%>% 
        layout(annotations=
                 list(x = 1, y = 0,
                      showarrow=F,
                      xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', 
                      xshift=0, yshift=0,
                      text=paste0('p=',
                                  t.test(long_neo_vector,long_paleo_vector)$p.value)),
               font=list(size=8))
    }
)
  
  #Scatter
  output$scatter<-renderPlotly(
    ggplotly(ggplot()+
        geom_point(data=master_by_species,aes_string(x=input$x_var_scatter, y=input$y_var_scatter, color="region", text="species"))+
        scale_color_manual(breaks=c('Neotropical','Paleotropical'),values=c('palegreen3','greenyellow'))+
        theme(axis.title.x=element_blank(),axis.title.y=element_blank()),
        tooltip = c("x","y", "text")) %>% 
        layout(annotations=
                 list(x = 1, y = 0,
                      showarrow=F,
                      xref='paper', yref='paper', 
                      xanchor='right', yanchor='auto', 
                      xshift=0, yshift=0,
                      text=paste0("R", "<sup>","2","</sup>","=",
                                (cor(master_by_species[[input$x_var_scatter]],
                                       master_by_species[[input$y_var_scatter]],
                                       use="pairwise.complete.obs"))^2)),
               font=list(size=8)))
    
  #Hist
  output$hist<-renderPlot(ggplot(data=master_by_species)+
                            stat_count(aes_string(x = input$x_var_hist, fill="region"), position='dodge', color='black')+
                            scale_fill_discrete(name = "region",labels = c("neotropical", "paleotropical"))+
                            scale_fill_manual(values=c('palegreen3','greenyellow'))+
                            scale_x_binned()+
                            theme(axis.title.x = element_blank())+
                            labs(caption=(paste0("p=",
                                                 ks.test(master_by_species_neo[[input$x_var_hist]],master_by_species_paleo[[input$x_var_hist]])$p.value,
                                                 " (Kolmogorov-Smirnov test)"))))
  #Map 
  output$map<-renderLeaflet(
    if (input$point_size=="falling velocity"){
    leaflet(gbif_master) %>% 
      addTiles() %>% 
      addCircleMarkers(lng=~decimalLongitude,
                       lat=~decimalLatitude,
                       stroke=FALSE,
                       color=~pal(region),
                       radius=~mean_velocity.mdx.25,
                       fillOpacity=0.5,
                       popup=paste0('<em>',gbif_master$species,'</em>',
                                    '<br>',
                                    gbif_master$decimalLongitude,
                                    ',',
                                    gbif_master$decimalLatitude,
                                    '<br>',
                                    "average falling velocity=",
                                    gbif_master$mean_velocity.md,
                                    "cm/s") %>% 
                         lapply(htmltools::HTML))
    }
    else if (input$point_size=="surface roughness"){
      leaflet(gbif_master) %>% 
        addTiles() %>% 
        addCircleMarkers(lng=~decimalLongitude,
                         lat=~decimalLatitude,
                         stroke=FALSE,
                         color=~pal(region),
                         radius=~mean_roughx50,
                         fillOpacity=0.5,
                         popup=paste0('<em>',gbif_master$species,'</em>',
                                      '<br>',
                                      gbif_master$decimalLongitude,
                                      ',',
                                      gbif_master$decimalLatitude,
                                      '<br>',
                                      "average surface roughness=",
                                      gbif_master$mean_rough) %>% 
                           lapply(htmltools::HTML))
    }
    else if (input$point_size=="height"){
      leaflet(gbif_master) %>% 
        addTiles() %>% 
        addCircleMarkers(lng=~decimalLongitude,
                         lat=~decimalLatitude,
                         stroke=FALSE,
                         color=~pal(region),
                         radius=~heightx.25,
                         fillOpacity=0.5,
                         popup=paste0('<em>',gbif_master$species,'</em>',
                                      '<br>',
                                      gbif_master$decimalLongitude,
                                      ',',
                                      gbif_master$decimalLatitude,
                                      '<br>',
                                      "average height=",
                                      gbif_master$height,
                                      "cm") %>% 
                           lapply(htmltools::HTML))
    }
    else if (input$point_size=="likely wind dispersal range"){
      leaflet(gbif_master) %>% 
        addTiles() %>% 
        addCircleMarkers(lng=~decimalLongitude,
                         lat=~decimalLatitude,
                         stroke=FALSE,
                         color=~pal(region),
                         radius=~mean_long,
                         fillOpacity=0.5,
                         popup=paste0('<em>',gbif_master$species,'</em>',
                                      '<br>',
                                      gbif_master$decimalLongitude,
                                      ',',
                                      gbif_master$decimalLatitude,
                                      '<br>',
                                      "likely wind dispersal range=",
                                      gbif_master$mean_long,
                                      "m") %>% 
                           lapply(htmltools::HTML))
    }
  )
}
