library(tidyr)
library(tidyverse)
library(dplyr)
library(broom)
library(readxl)
library(ggplot2)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"


calluna.raw <-  read_excel(path = "data/rawdata.xlsx", sheet = "calluna", col_names = TRUE)

calluna <- calluna.raw %>% 
  select(site, year, Karbon, Nitrogen) %>% 
  mutate(Year = year) %>% 
  mutate(Year = recode(Year, "1995" = "0", "1998" = "3", "2003" = "8", "2008" = "13", "2013" = "18", "2018" = "23", "2023" = "28")) %>% 
  mutate(Year = as.numeric(Year))  
  #mutate(site = as.factor(site)) 


callunaLM<- calluna %>% 
  gather("nutrient", "measurement", Karbon, Nitrogen) 







###################

 
  mutate(Fosfor = as.numeric(gsub(pattern="<", replacement="", Fosfor))) %>% 
  mutate(Kalium = as.numeric(gsub(pattern="<", replacement="", Kalium))) %>% 
  mutate(Magnesium = as.numeric(gsub(pattern="<", replacement="", Magnesium))) %>% 
  mutate(Kalsium = as.numeric(gsub(pattern="<", replacement="", Kalsium))) %>% 
  mutate(Natrium = as.numeric(gsub(pattern="<", replacement="", Natrium)))
