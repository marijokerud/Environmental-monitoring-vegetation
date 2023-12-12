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
  select(-P) %>% #only values for 2023
  gather("type", "dose", pH, N_Kjeldahl, C_N, K, Mg, Na_AL) %>% 
  group_by(year, type) %>% 
  summarise(avg=mean(dose), SD=sd(dose)) 


ggplot(soilPLOT, aes(x=year, y=avg)) + 
  geom_line(lwd=1, position = position_dodge(width = 0.4)) +
  geom_point(cex=3, position = position_dodge(width = 0.4)) +
  geom_linerange(aes(ymin=avg-(SD*2/sqrt(6)), ymax=avg+(SD*2/sqrt(6))), position = position_dodge(width = 0.4)) +
  facet_grid(type~., scales="free_y") +
  ylab("") +
  xlab("År") +
  scale_x_continuous(breaks=c(1995, 1998, 2003, 2008, 2013, 2018, 2023)) +
  #scale_colour_manual(values=cbbPalette) +
  theme_bw() +
  theme(legend.position="none") +
  theme(strip.text.y = element_text(size = 12),
        axis.title.x = element_text(size=12,hjust=0.5),
        axis.title.y = element_text(size=12,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text( color="black", size=18),
        legend.text = element_text( color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank())
