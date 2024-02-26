library(lme4)
library(xlsx)

outputCalluna = callunaLM %>% 
  # replace group_by() with nest_by() 
  # to convert your model data to a vector of lists
  nest_by(nutrient) %>%
  # change do() to mutate(), then add list() before your model
  # make sure to change data = .  to data = data
  mutate(fitSoil = list(lm(measurement ~ Year, data = data))) %>%
  reframe(tidy(fitSoil))

outputCalluna

resCalluna = callunaLM %>% 
  nest_by(nutrient) %>%
  mutate(fitSoil = list(lm(measurement ~ Year, data = data))) %>%
  reframe(glance(fitSoil))

resCalluna

#write.xlsx(resSoil, file = "output/outputSoil.xlsx", col.names = TRUE, row.names = TRUE)

###############################
summary(lm(Kalium~Year,data=soil))
summary(lm(Kalsium~Year,data=soil))
summary(lm(Magnesium~Year,data=soil))
summary(lm(Natrium~Year,data=soil))
summary(lm(LOI~Year,data=soil))
summary(lm(TOC~Year,data=soil))
summary(lm(Nitrogen~Year,data=soil))
summary(lm(C_N~Year,data=soil))



