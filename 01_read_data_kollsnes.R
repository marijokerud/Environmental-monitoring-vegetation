#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

species <- read_excel(path = "data/rawdata.xlsx", sheet = "vegetation", col_names = TRUE)
