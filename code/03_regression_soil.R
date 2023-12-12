library(lme4)
#mod <- datLM %>% 
#group_by(heavymetal) %>% 
#do(fit = lm(dose ~  aar, data = .))

#names(mod)
#res.mod<-tidy(mod, fit)
#tibble(res.ratio)<-tidy(result.ratio, fit)

summary(lm(pH~year,data=soil))
summary(lm(N_Kjeldahl~year,data=soil))
summary(lm(C_N~year,data=soil))

#PLOT
soilPLOT<- soil %>%
  gather("type", "dose", pH, N_Kjeldahl, C_N) %>% 
  group_by(Aar, heavymetal) %>% 
  summarise(avg=mean(dose), SD=sd(dose)) %>% 
  mutate(Provetype = "samlet")