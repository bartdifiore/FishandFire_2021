library(piecewiseSEM)
library(tidyverse)
library(lme4)
library(lmerTest)

forround <- c("och", "grazer", "coll_gath", "shredder")

df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 


# the model exactly described by the hypothesized DAG

detrital_sem <- psem(
  glmer(trout ~ temp + avg_max_depth + flow + do + (1|code) + (1|year), df, family = binomial(link = "logit")),
  lmer(avg_max_depth ~ flow + (1|code) + (1|year), df),
  glmer(shredder ~ temp + avg_max_depth + do + leaf_cpom_wet_wt + (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(temp ~ avg_canopy_cover + flow + (1|code) + (1|year), df),
  lmer(do ~ flow + temp + (1|code) + (1|year), df), 
  glmer(leaf_cpom_wet_wt ~ temp + avg_canopy_cover + flow + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(detrital_sem, conserve = T)
detrital_sem2 <- update(detrital_sem, avg_max_depth %~~% do)
detrital_sem3 <- update(detrital_sem2, trout %~~% shredder)
detrital_sem4 <- update(detrital_sem3, avg_max_depth %~~% avg_canopy_cover)

summary(detrital_sem4, conserve = T)
out <- coefs(detrital_sem4, standardize.type = "latent.linear")
plot(detrital_sem4)

write_csv(out, "detrital_sem_output.csv")


# the selected model based on D-separation tests

detrital_sem_dtest <- psem(
  glmer(trout ~ temp + avg_max_depth + flow + do + (1|code) + (1|year), df, family = binomial(link = "logit")),
  lmer(avg_max_depth ~ flow + (1|code) + (1|year), df),
  glmer(shredder ~ temp + avg_max_depth + do + leaf_cpom_wet_wt + flow + (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(temp ~ avg_canopy_cover + flow + (1|code) + (1|year), df),
  lmer(do ~ flow + temp + (1|code) + (1|year), df), 
  glmer(leaf_cpom_wet_wt ~ temp + avg_canopy_cover + flow + avg_max_depth + (1|code) + (1|year), df, family = poisson(link = "log"))
)

detrital_sem_dtest2 <- update(detrital_sem_dtest, avg_max_depth %~~% do)
detrital_sem_dtest3 <- update(detrital_sem_dtest2, trout %~~% shredder)
detrital_sem_dtest4 <- update(detrital_sem_dtest3, avg_max_depth %~~% avg_canopy_cover)

summary(detrital_sem_dtest4, conserve = T)

plot(detrital_sem_dtest4)

out4 <- coefs(detrital_sem_dtest4, standardize.type = "latent.linear")
write_csv(out4, "detrital_sem_output_dsep.csv")

















