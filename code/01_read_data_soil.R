library(tidyr)
library(tidyverse)
library(dplyr)
library(broom)
library(readxl)
library(ggplot2)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"


soil.raw <-  read_excel(path = "data/rawdata.xlsx", sheet = "soil", col_names = TRUE)

soil <- soil.raw %>% 
  select(site, year, pH, Fosfor, Kalium, Magnesium, Kalsium, Natrium, LOI, Karbon, Nitrogen, CN) %>% 
  mutate(Year = year) %>% 
  mutate(Year = recode(Year, "1995" = "0", "1998" = "3", "2003" = "8", "2008" = "13", "2013" = "18", "2018" = "23", "2023" = "28")) %>% 
  mutate(Year = as.numeric(Year))  
  #mutate(site = as.factor(site)) 


soilLM<- soil %>% 
  select(-Fosfor) %>% 
  gather("element", "measurement", Kalium, Kalsium, Magnesium, Natrium, pH, LOI, Karbon, Nitrogen, CN) 


soilaverage <- soilLM %>% 
  #filter(year == "2023") %>% 
  select(site, element, measurement) %>% 
  group_by(site, element) %>% 
  summarise(average = mean(measurement))






###################

 
  mutate(Fosfor = as.numeric(gsub(pattern="<", replacement="", Fosfor))) %>% 
  mutate(Kalium = as.numeric(gsub(pattern="<", replacement="", Kalium))) %>% 
  mutate(Magnesium = as.numeric(gsub(pattern="<", replacement="", Magnesium))) %>% 
  mutate(Kalsium = as.numeric(gsub(pattern="<", replacement="", Kalsium))) %>% 
  mutate(Natrium = as.numeric(gsub(pattern="<", replacement="", Natrium)))
