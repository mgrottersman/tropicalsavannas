setwd("~/SCHOOL/research/cerrado serengeti stuff/shiny app")
library(shiny)

intro_panel<-tabPanel(
  "Introduction",
  h1("Variation in Grass Seed Dispersal in Tropical Savannas"),
  h3(HTML(paste0("By Maria Rottersman", tags$sup("1,2"),
            ", William H. Brightly",tags$sup("1,2"),
            ", Matheus E. Bianconi",tags$sup("3"),
            ", and  Caroline A.E. Str", "&ouml","mberg",tags$sup("1,2")))),
  h5(HTML(paste0(tags$sup("1"),"University of Washington Department of Biology"))),
  h5(HTML(paste0(tags$sup("2"),"Burke Museum of Natural History and Culture"))),
  h5(HTML(paste0(tags$sup("3"),"University of Sheffield Department of Animal and Plant Sciences"))),
  img(src = "lab_logo.png", height = "30%", width = "30%"),
  img(src = "burke_logo.png", height = "25%", width = "25%"),
  p("Savannas of the neotropics and paleotropics, although similar in their C4 
    grass dominated understory, possess distinct evolutionary and environmental 
    histories which have contributed to structural differences between these 
    communities. These differences are likely to have major impacts on the 
    ecology of each region, including seed dispersal processes. Seed dispersal 
    is an important component of the plant lifecycle, and angiosperms have 
    evolved multiple ways of dispersing their progeny. We test the hypothesis 
    that grasses in open habitats are more likely to favor wind dispersal 
    (anemochory) and that grasses in areas with dense megafauna are more likely 
    to favor animal dispersal (epizoochory and endozoochory) by comparing 
    morphological traits of ecologically dominant grasses of the tropical 
    savannas of Venezuela, Cerrado (sensu stricto) of Brazil, and the Serengeti 
    region in Tanzania. Higher canopy cover in regions of neotropical savannas 
    relative to paleotropical savannas lead to the prediction that anemochory 
    is favored in the latter. The abundance of megafauna in the Serengeti, 
    particularly grazing ungulates, suggests that epizoochory and endozoochory 
    are more prominent in paleotropical savannas than in neotropical savannas, 
    which are generally lacking in megafauna. To test our hypotheses, we sample 
    dispersal structures (diaspores) from herbarium specimens. To estimate wind 
    dispersal ability, we measure falling velocity by taking high-speed videos 
    of falling diaspores. Plant height data was collected for each species from Kew. 
    Using falling velocity and plant height, we are able 
    to compute likely wind dispersal range from the parent plant.
    To measure capacity for epizoochory, we take high definition photos of seeds
    and then compare their outlines to their convex hulls. Specimens with a 
    lower ratio of convex hull perimeter to outline perimeter are rougher, and 
    therefore more likely to stick to a passing animal."),
  img(src = "ch_image.png", height = "25%", width = "25%"),
  p("Contrary
    to predictions, our phylogenetically informed multilevel model indicated no
    effect of region on capacity for wind dispersal. We decided to test whether
    our similar dispersal distance values were because of trait convergence in 
    savanna grasses. We ran model fitting analyses to test whether the evolution
    of wind dispersal traits among grasses was better explained by treating 
    savanna dominants as a separate evolutionary regime, but strongest support 
    was for a model which included them with the rest of grasses."),
  img(src = "phylogeny_image.png", height = "50%", width = "50%"),
  p("We can 
    speculate about why our results differed from our initial predictions. 
    Since the neotropics had many more large animals during the Pleistocene era,
    it may be possible that having seeds equipped for epizoochory was a feature
    that evolved in an ancient environment  and was not lost over time. The 
    Cerrados have ecologically diverse subregions with varying canopy cover, 
    which may explain why we see comparable average wind dispersal ranges in 
    both regions, but our distribution of values looks different. As we continue
    to examine trends in grass seed dispersal in tropical savannas, we will be 
    measuring additional diaspore traits.")
  )
