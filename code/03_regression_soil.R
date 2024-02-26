library(lme4)
library(xlsx)

outputSoil = soilLM %>% 
  # replace group_by() with nest_by() 
  # to convert your model data to a vector of lists
  nest_by(element) %>%
  # change do() to mutate(), then add list() before your model
  # make sure to change data = .  to data = data
  mutate(fitSoil = list(lm(measurement ~ Year, data = data))) %>%
  reframe(tidy(fitSoil))

outputSoil

resSoil = soilLM %>% 
  nest_by(element) %>%
  mutate(fitSoil = list(lm(measurement ~ Year, data = data))) %>%
  reframe(glance(fitSoil))

resSoil

write.xlsx(resSoil, file = "output/outputSoil2.xlsx", col.names = TRUE, row.names = TRUE)




###################
summary(lm(Kalium~Year,data=soil))
summary(lm(Kalsium~Year,data=soil))
summary(lm(Magnesium~Year,data=soil))
summary(lm(Natrium~Year,data=soil))
summary(lm(LOI~Year,data=soil))
summary(lm(TOC~Year,data=soil))
summary(lm(Nitrogen~Year,data=soil))
summary(lm(C_N~Year,data=soil))



