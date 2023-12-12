library(lme4)

site.scores <- site.scores %>% 
  mutate()

model <- lmer(NMDS1~ year + felt_id + (1 | felt_id),  
            data = site.scores)

model <- lm(NMDS1~ year + I(sqrt(year)),  
              data = site.scores)

model <- lmer(NMDS1 ~ year * (1|felt_id), data = site.scores)


summary(model)

# Plot the residual vs fitted values
plot(model, which = 1)

# Plot the Q-Q plot of residuals
plot(model, which = 2)


qqnorm(residuals(model))

qqline(residuals(model))
