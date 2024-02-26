library(lme4)

model1 <- lm(NMDS1~ year3 + I(sqrt(year3)),  
            data = site.scores)

model2 <- lm(NMDS2~ year3 + I(sqrt(year3)),  
            data = site.scores)

summary(model1)
summary(model2)

model3 <- lm(NMDS1~ year3,  
             data = site.scores)

model4 <- lm(NMDS2~ year3,  
             data = site.scores)

summary(model3)
summary(model4)
AIC(model1, model2, model3, model4)

# Plot the residual vs fitted values
plot(model, which = 1)

# Plot the Q-Q plot of residuals
plot(model, which = 2)


qqnorm(residuals(model))

qqline(residuals(model))

###
model <- lmer(NMDS1~ year + felt_id + (1 | felt_id),  #Model failed to converge
              data = site.scores)
model <- lmer(NMDS1 ~ year * (1|felt_id), data = site.scores) #I dont think felt_id can be a random factor because we have too few sites
