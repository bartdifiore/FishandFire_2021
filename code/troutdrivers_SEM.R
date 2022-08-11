library(piecewiseSEM)
library(tidyverse)
library(lme4)
library(lmerTest)

forround <- c("och", "grazer", "coll_gath", "shredder")

df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 

# Model of hypothesized DAG
trout_sem <- psem(
  glmer(trout ~ coll_gath + do + temp + flow + avg_daily_disch_nr_nrst_gage + biotic_index + avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(coll_gath ~ algae_cpom_wet_wt + (1|code) + (1|year), df, family = poisson(link = "log")),
  lmer(do ~ algae_cpom_wet_wt + temp + flow + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df),
  glmer(algae_cpom_wet_wt ~ avg_canopy_cover + flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(biotic_index ~ flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df)
)

summary(trout_sem, conserve = T)
plot(trout_sem)


# Model based on d-separation tests

trout_sem_dsep <- psem(
  glmer(trout ~ coll_gath + do + temp + flow + avg_daily_disch_nr_nrst_gage + biotic_index + avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(coll_gath ~ algae_cpom_wet_wt + temp + flow + avg_daily_disch_nr_nrst_gage + biotic_index + avg_max_depth + avg_canopy_cover + (1|code) + (1|year), df, family = poisson(link = "log")),
  lmer(do ~ algae_cpom_wet_wt + temp + flow + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df),
  glmer(algae_cpom_wet_wt ~ avg_canopy_cover + flow + avg_daily_disch_nr_nrst_gage + temp + avg_max_depth + biotic_index + (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(biotic_index ~ flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df)
)
summary(trout_sem_dsep, conserve = T)
plot(trout_sem_dsep)






# Model w/out coll_gath
trout_sem <- psem(
  glmer(trout ~ do + temp + flow + avg_daily_disch_nr_nrst_gage + biotic_index + avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  lmer(do ~ algae_cpom_wet_wt + temp + flow + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df), 
  glmer(algae_cpom_wet_wt ~ avg_canopy_cover + flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(biotic_index ~ flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df)
)

summary(trout_sem, conserve = T)
plot(trout_sem)

out <- coefs(trout_sem, standardize.type = "latent.linear")
write_csv(out, "trout_sem_output.csv")



# Model w/out coll_gath based on d-sep tests
trout_sem_dsep <- psem(
  glmer(trout ~ do + temp + flow + avg_daily_disch_nr_nrst_gage + biotic_index + avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  lmer(do ~ algae_cpom_wet_wt + temp + flow + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df),
  glmer(algae_cpom_wet_wt ~ avg_canopy_cover + flow + avg_daily_disch_nr_nrst_gage + avg_max_depth + biotic_index + temp +  (1|code) + (1|year), df, family = poisson(link = "log")), 
  lmer(biotic_index ~ flow + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df)
)


summary(trout_sem_dsep, conserve = T)
plot(trout_sem_dsep)

out <- coefs(trout_sem_dsep, standardize.type = "latent.linear")
write_csv(out, "trout_sem_output_dsep.csv")





