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

  
#### COMMUNITY MATRIX
species.mat<- speciesdata2 %>% 
  as.data.frame

species.mat<- matrify(species.mat)
species.matrix <- species.mat 

### Find number of species in functional groups across years
abundance.data2 <- speciesdata %>% 
  left_join(func.type, by="species") %>% 
  select(locality, species, year, func_type2) %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(func_type2) %>% 
  spread(key = year, value = n)

abundance.data3 <- speciesdata %>% 
  left_join(func.type, by="species") %>% 
  select(locality, species, year, func_type3) %>% 
  distinct() %>%
  group_by(year) %>% 
  count(func_type3) %>% 
  spread(key = year, value = n)

write.xlsx(abundance.data2, file = "output/abundance_data2.xlsx", col.names = TRUE, row.names = TRUE)
write.xlsx(abundance.data3, file = "output/abundance_data3.xlsx", col.names = TRUE, row.names = TRUE) 

# Check if something is strange with blank space
#mutate(species2 = gsub("\\s+", "_", perl=TRUE, species)) %>% 

### Find dominating species across years
species.numbers <- speciesdata %>% 
  left_join(func.type, by="species") %>% 
  select(felt_id, species, year, func_type2) %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(species) %>% 
  spread(key = year, value = n)

species.numbers2 <- speciesdata %>% 
  left_join(func.type, by="species") %>% 
  select(felt_id, species, year, func_type2) %>% 
  distinct() %>% 
  group_by(year) %>% 
  count(felt_id) %>% 
  spread(key = year, value = n)

write.xlsx(species.numbers2, file = "output/species.numbers2.xlsx", col.names = TRUE, row.names = TRUE) 
