### laster biblioteker
library(tidyverse)
library(readxl)
library(ggplot2)
#library(xlsx)
library(openxlsx)

#library(broom)
#library(tidypredict)
#library(scale)

### importerer data
#Bruk datasettene fra Vegetasjonsdata.R 



#################### GULMYRAN #####################

abundance.data <- speciesdata %>%
  filter(year == "2008" | year == "2023") %>% 
  complete(year, nesting(locality, felt_id, species), fill = list(abundance = 0)) %>% 
  mutate(year3 = year) %>%
  mutate(year = str_replace(year, "2008", "1")) %>% 
  mutate(year = str_replace(year, "2023", "2"))


#TEST ENDRING I DEKNINGSDATA FRA FØRSTE ÅR TIL SISTE ÅR
abundance.change <- abundance.data %>% 
  group_by(species, felt_id) %>% #OMR_navn, 
  do(w = wilcox.test(abundance ~ year3, data=., paired=TRUE, alternative = "two.sided")) %>% 
  summarise(abundance, felt_id, Wilcox = w$p.value)

write.xlsx(resDEKN_GUL, file = "2020/Dekning_wilcox_GUL.xlsx", col.names = TRUE, row.names = TRUE) 


#################### LOMSTJONNA #####################
dekningsdata_LOM <- vegetasjon_LOM %>% #Velg første og siste analyseår
  filter(AAR == "2018" | AAR == "2020") %>% 
  select(Felt_id, Rute_id, AAR, NorskNavnNINA, GjeldendeFloranavnNINA, Dekning, OMR_navn, Veg_type) %>%
  complete(AAR, nesting(NorskNavnNINA, Rute_id, GjeldendeFloranavnNINA, Felt_id, 
                        OMR_navn, Veg_type), fill = list(Dekning = 0)) #%>%  #fill inn species with 0 occurences





resDEKN_LOM <- dekningsdata_LOM %>% 
  group_by(NorskNavnNINA, Felt_id) %>% #OMR_navn, 
  do(w = wilcox.test(Dekning ~ AAR, data=., paired=TRUE, alternative = "two.sided")) %>% 
  summarise(NorskNavnNINA, Felt_id, Wilcox = w$p.value)

write.xlsx(resDEKN_LOM, file = "2020/Dekning_wilcox_LOM.xlsx", col.names = TRUE, row.names = TRUE) 


### Finn arter som har gått frem eller tilbake
#Trenger ikke kjøre denne siden det ikke er noen arter som er signifikante.

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



######################## GAMLE KODER ########################



vegetasjonGUL <- vegetasjon %>% 
  filter(OMR_NAVN == "Gulmyran") %>% 
  filter(AAR == "2008" | AAR == "2020") 
#mutate(ar = str_replace(ar, "2008", "1")) %>% 
#mutate(ar = str_replace(ar, "2020", "2"))


vegetasjonLOM <- vegetasjon %>%
  filter(OMR_navn == "Lomtjonna") %>%
  filter(AAR == "2018" | AAR == "2020") #%>% 
#mutate(ar = AAR) %>% 
#mutate(ar = str_replace(ar, "2018", "1")) %>% 
#mutate(ar = str_replace(ar, "2020", "2"))


mutate(ar = AAR) %>% 
  mutate(ar = str_replace(ar, "2008", "1")) %>% 
  mutate(ar = str_replace(ar, "2018", "1")) %>% 
  mutate(ar = str_replace(ar, "2020", "2"))

### importerer data
#snohvit_old <- read_excel(path = "Veg_data_S_2006_2008_2013.xlsx", sheet = NULL, col_names = TRUE)
snohvit<- read_excel(path = "Snøhvit/Vegetasjonsdata/Uttrekk vegetasjonsdata Snøhvit 2006-2018.xlsx", sheet = "Frekvens_dekn_2006-2018", col_names = TRUE)
#vegetasjon<- read_excel(path = "Snøhvit/Vegetasjonsdata/Uttrekk vegetasjonsdata Snøhvit 2006-2018.xlsx", sheet = "Frekvens_dekn_2006-2018", col_names = TRUE)
names(snohvit)

#Endre Dekn til Dekning i snøvitdatasettet
snohvit <- snohvit %>% 
  rename(Dekning = Dekn)


# Trekk ut 2018 data
snohvit18dataD <-  snohvit %>% 
  select(Felt_id, Rute_id, AAR, NorskNavnNINA, GjeldendeFloranavnNINA, BESKRIVELSE, Dekning) %>% 
  filter(AAR == "2018") %>% 
  mutate(Dekning18 = Dekning) %>% 
  select(-AAR, -Dekning)

# Trekk ut 2006 data og gjør en full sammenslåing av 2006 og 2018
snohvitdataD <-  snohvit %>% 
  select(Felt_id, Rute_id, AAR, NorskNavnNINA, BESKRIVELSE, GjeldendeFloranavnNINA, Dekning) %>% 
  filter(AAR == "2006") %>% 
  select(-AAR) %>% 
  full_join(snohvit18dataD)

#Trekk ut 2006 data fra det fullstendige datasettet
snohvit06D <- snohvitdataD %>% 
  select(-Dekning18) %>% 
  mutate(AAR = 2006)

#Trekk ut 2018 data fra det fullstendige datasettet
snohvit18D <- snohvitdataD %>% 
  select(-Dekning) %>% 
  mutate(AAR = 2018) %>% 
  rename("Dekning" = "Dekning18")


snohvitDEKN <-  snohvit06D %>% 
  bind_rows(snohvit18D) %>% 
  replace_na(list(Dekning = 0))


resDEKN <- snohvitDEKN %>% 
  group_by(NorskNavnNINA, Felt_id) %>% 
  do(w = wilcox.test(Dekning ~ AAR, data=., paired=TRUE, alternative = "two.sided")) %>% 
  summarise(NorskNavnNINA, Felt_id, Wilcox = w$p.value)

write.csv2(resDEKN, file = "Result_Dekning.csv") 


### Finn arter som har gått frem eller tilbake

endringPLUSS<- snohvitdataD %>% 
  replace_na(list(Dekning = 0)) %>% 
  replace_na(list(Dekning18 = 0)) %>% 
  mutate(endring = Dekning18 - Dekning) %>% 
  filter(endring>=1) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA) %>% 
  rename("pluss" = "n")

endringMINUS<- snohvitdataD %>% 
  replace_na(list(Dekning = 0)) %>% 
  replace_na(list(Dekning18 = 0)) %>% 
  mutate(endring = Dekning18 - Dekning) %>% 
  filter(endring<0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("minus" = "n")

endringNULL<- snohvitdataD %>% 
  replace_na(list(Dekning = 0)) %>% 
  replace_na(list(Dekning18 = 0)) %>% 
  mutate(endring = Dekning18 - Dekning) %>% 
  filter(endring==0) %>% 
  group_by(Felt_id) %>%
  count(NorskNavnNINA)%>% 
  rename("null" = "n")

endringD <- endringPLUSS %>% 
  full_join(endringMINUS) %>% 
  full_join(endringNULL)







#######################
#forsol_heiA<- snohvit %>% 
select(Felt_id, Rute_id, AAR, NorskNavnNINA, Dekning, Dekn) %>% 
  filter(Felt_id == "Sn A") %>% 
  group_by(NorskNavnNINA, AAR) %>% 
  summarise(
    count = n(),
    median = median(Dekning, na.rm = TRUE),
    IQR = IQR(Dekning, na.rm = TRUE)
  )



