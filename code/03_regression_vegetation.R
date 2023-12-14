library(lme4)

site.scores <- site.scores %>% 
  mutate()

model <- lm(NMDS1~ year + I(sqrt(year)),  
            data = site.scores)

summary(model)

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
