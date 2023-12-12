#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

rawdata <- read_excel(path = "data/rawdata.xlsx", sheet = "vegetation", col_names = TRUE)
func.type <-  read_excel(path = "data/rawdata.xlsx", sheet = "func_type", col_names = TRUE)

#Find all species
species.name <- rawdata %>% 
  select(species) %>% 
  distinct()

norwegian.name <- func.type %>% 
  select(species, norwegian) %>% 
  drop_na(norwegian)
  

speciesdata <- rawdata %>% 
  select(locality, felt_id,species, abundance, year) %>% 
  drop_na(abundance) %>% #loose the NAs in the data
  mutate(abundance = gsub("+", "0.25", abundance, fixed = TRUE)) %>%    #Gi + verdi på 0.25
  mutate(abundance = as.numeric(abundance))
  
#Aggregate all mosses and lichens
mossesandlichens <- speciesdata %>% 
  left_join(func.type, by="species") %>%
  unite("community", felt_id, year, remove = FALSE) %>% 
  drop_na(func_type) %>%
  mutate(abundance = as.numeric(abundance)) %>% 
  group_by(community, func_type) %>% 
  summarise(abundance = sum(abundance)) %>% 
  filter(func_type == "bryophyte" | func_type == "liverwort" | func_type == "lichen") %>% 
  rename(species = func_type)

speciesdata2 <- speciesdata %>% 
  left_join(func.type, by="species") %>% 
  filter(func_type == "fern" | func_type == "grass" | func_type == "herb" | func_type == "segde" | func_type == "shrub_dwarf"| 
           func_type == "bryophytes" | func_type == "liverworts" | func_type == "lichens") %>% 
  unite("community", felt_id, year) %>% 
  select(community, species, abundance) %>% 
  bind_rows(mossesandlichens)

  
#### COMMUNITY MATRIX every 10 m
species.mat<- speciesdata2 %>% 
  as.data.frame

species.mat<- matrify(species.mat)
species.matrix <- species.mat 




# Check if something is strange with blank space
#mutate(species2 = gsub("\\s+", "_", perl=TRUE, species)) %>% 
