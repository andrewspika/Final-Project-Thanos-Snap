library(tidyverse)
library(ggplot2)
library(tidyverse)
library(dslabs)
library(gridExtra)
library(ggrepel)
library(ggthemes)
library(knitr)
library(viridis)
library(stringr)
library(rvest)
library(hexbin)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)
library(ggforce)

# Function to create a generation of offspring, or initial population
# Inputs: generation size 'size', alleles in genetic pool 'alleles', and allele proportion 'allele.prop'
# Outputs: tibble with a generation of offspring
create.generation <- 
  function(size=100, alleles=c("AA", "Aa", "aa"), allele.prop=c(.25,.5,.25)) {
    gen <- tibble(Trait = NA, Allele = NA, .rows = size)
    gen$Allele <- sample(alleles, size = size, prob = allele.prop, replace = TRUE)
    for(index in 1:nrow(gen)) {
      if(gen$Allele[index] == "AA" | gen$Allele[index] == "Aa") {
        gen$Trait[index] <- "Dominant"
      } else {
        gen$Trait[index] <- "Recessive"
      }
    }
    return(gen)
  }


# Function to induce bottleneck event and alter population
# Inputs: tibble with population to affect 'popu', and type of bottleneck event 'event', survival rate input 'survival.rate' changes survival rate of pathogen for each characteristic and how drastic natural distasters are
# Outputs: the tibble with the resulting population (the survivors of the population)
bottleneck <- function(popu, event, survival.rate) {
  if(event == "Founder" ) {
    survival.rate <- NA
    deaths <- sample(1:nrow(popu), size = nrow(popu)*.9)
    
  } else if(event == "Dominant Characteristic Pathogen") {
    dom.index <- which(popu$Trait=="Dominant")
    deaths <- sample(dom.index, size = survival.rate*length(dom.index))
    
  } else if(event == "Recessive Characteristic Pathogen") {
    rec.index <- which(popu$Trait=="Recessive")
    deaths <- sample(rec.index, size = survival.rate*length(rec.index))
    
  } else if(event == "Natural Disaster") {
    deaths <- sample(1:nrow(popu), size = survival.rate*nrow(popu))
    
  } else if(event == "Thanos Snap") {
    deaths <- sample(1:nrow(popu), size = .5*nrow(popu))
  }
  
  affected <- popu[-deaths,]
  return(affected)
}

# Function to get alleles and allele proportions from population
# Inputs: tibble with alleles and proportions 'pop'
# Outputs: tibble with the alleles and their respective proportions from population
get.proportions <- function(pop) {
  proportions <- pop %>% group_by(Allele) %>% summarize(prop = length(Allele)/nrow(pop))
  return(proportions)
}
  
  
## Sample simulation
# Function to simulate the generations
# Inputs: number of generations 'gens', default is 10, the starting allele frequency 'start.freq' ordered respective to 'AA', 'Aa', 'aa'. Also population size 'pop.size' and bottleneck event 'bottle' and survival rate for an event, specific to characteristic if either pathogen is chosen 'surv.rate'
# Outputs: the resulting final generation of the simulation in a tibble
simulate.generations <- function(gens=10, start.freq=c(.25, .5, .25), pop.size=100, bottle="Founder", surv.rate = NA) {
  population <- create.generation(size = pop.size, allele.prop = start.freq)
 # population <- population %>% bottleneck(event = bottle, survival.rate = surv.rate)
  all.gens <- population %>% mutate(Gen = 1)
  for(gen in 2:gens) {
    props <- population %>% get.proportions()
   
    # Alteration
     if(gen == gens/2) {
      population <- create.generation(size = pop.size, alleles=props$Allele, allele.prop=props$prop) %>% 
        bottleneck(event = bottle, survival.rate = surv.rate)
    } else {
    
     if(nrow(population) < pop.size) {
       population <- create.generation(size = (nrow(population)+0.10*pop.size), alleles=props$Allele, allele.prop=props$prop)
     } else {
       population <- create.generation(size = pop.size, alleles=props$Allele, allele.prop=props$prop)
     }
    }
    population <- population %>% mutate(Gen = gen)
    all.gens <- all.gens %>% rbind(population)
  }
  return(all.gens)
}
  

  
  