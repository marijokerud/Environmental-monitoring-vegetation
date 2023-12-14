library(tidyverse)
library(readxl)
library(ggplot2)


#Bruk datasettet fra 01_read_data_kollsnes.R 


abundance.data <- speciesdata %>%
  filter(year == "2008" | year == "2023") %>% 
  complete(year, nesting(locality, felt_id, species), fill = list(abundance = 0)) %>% 
  mutate(year3 = year) %>%
  mutate(year3 = str_replace(year3, "2008", "1")) %>% 
  mutate(year3 = str_replace(year3, "2023", "2"))


#TEST ENDRING I DEKNINGSDATA FRA FØRSTE ÅR TIL SISTE ÅR
abundance.change <- abundance.data %>% 
  group_by(species) %>% # , felt_id, OMR_navn, 
  do(w = wilcox.test(abundance ~ year, data=., paired=TRUE, alternative = "two.sided")) %>% 
  summarise(species, Wilcox = w$p.value)

write.xlsx(abundance.change, file = "output/abundance.xlsx", col.names = TRUE, row.names = TRUE) 


 


### Finn arter som har gått frem eller tilbake
#Trenger ikke kjøre denne siden det ikke er noen arter som er signifikante. Må tilpasses Kollsnes i fremtida.

########GULMYRAN########## Felt F, G, H, K, L, de to siste fra 2010 til 2020
#denne koden må oppdateres hvis det er signifikant endring for tuemyrene (K,L), dvs bruke aar 1 og 2.

endringPLUSS_GUL<- dekningsdata_GUL %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2008`) %>% 
  filter(endring>=1) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA) %>% 
  rename("pluss" = "n")

endringMINUS_GUL<- dekningsdata_GUL %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2008`) %>%
  filter(endring<0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("minus" = "n")

endringNULL_GUL<- dekningsdata_GUL %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2008`) %>%
  filter(endring==0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("null" = "n")

endringDEKN_GUL <- endringPLUSS_GUL %>% 
  full_join(endringMINUS_GUL) %>% 
  full_join(endringNULL_GUL) %>% 
  replace_na(list(pluss = 0, minus = 0, null = 0))

write.xlsx(as.data.frame(endringDEKN_GUL), file = "2020/Dekning_endring_Gulmyran.xlsx", col.names = TRUE, row.names = TRUE) 

########Lomtjonna##########

endringPLUSS_LOM<- dekningsdata_LOM %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2018`) %>% 
  filter(endring>=1) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA) %>% 
  rename("pluss" = "n")

endringMINUS_LOM<- dekningsdata_LOM %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2018`) %>%
  filter(endring<0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("minus" = "n")

endringNULL_LOM<- dekningsdata_LOM %>%
  spread(key = AAR, value = Dekning) %>% 
  mutate(endring = `2020`-`2018`) %>%
  filter(endring==0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("null" = "n")

endringDEKN_LOM <- endringPLUSS_LOM %>% 
  full_join(endringMINUS_LOM) %>% 
  full_join(endringNULL_LOM) %>% 
  replace_na(list(pluss = 0, minus = 0, null = 0))

write.xlsx(as.data.frame(endringDEKN_LOM), file = "2020/Dekning_endring_Lomstjonna.xlsx", col.names = TRUE, row.names = TRUE) 



