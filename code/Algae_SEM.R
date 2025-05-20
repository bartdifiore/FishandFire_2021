source("code/libraries_functions.R")
library(piecewiseSEM)

forround <- c("och", "grazer", "coll_gath", "shredder")

df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris == 0, month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 

# model of apriori hypothesized DAG
algae_sem <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + algae_cpom_wet_wt + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + algae_cpom_wet_wt + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(algae_sem, conserve = T)
plot(algae_sem)

out <- coefs(algae_sem, standardize.type = "latent.linear")
write_csv(out, "algae_sem_output.csv")



# model based on d-sep tests

algae_sem_dsep <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(och ~ trout + algae_cpom_wet_wt + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + algae_cpom_wet_wt + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  glmer(grazer ~ trout + och + avg_max_depth + algae_cpom_wet_wt + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ avg_daily_disch_nr_nrst_gage + avg_max_depth + trout + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(algae_sem_dsep, conserve = T)
plot(algae_sem_dsep)

AIC(algae_sem, algae_sem_dsep)


out3 <- coefs(algae_sem_dsep, standardize.type = "latent.linear")
write_csv(out3, "algae_sem_output_dsep.csv")




# Fully top down

algae_sem_topdown <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ grazer + coll_gath + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(algae_sem_topdown, conserve = T)

algae_sem_topdown_dsep <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ grazer + coll_gath + och + trout + avg_daily_disch_nr_nrst_gage + avg_max_depth + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(algae_sem_topdown_dsep, conserve = T)

AIC(algae_sem, algae_sem_dsep, algae_sem_topdown, algae_sem_topdown_dsep)


# Fully bottom up

algae_sem_bottomup <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df, family = binomial(link = "logit")),
  glmer(och ~ coll_gath + avg_max_depth + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(coll_gath ~ algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df, family = poisson(link = "log")), 
  glmer(grazer ~ algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df, family = poisson(link = "log"))
)

summary(algae_sem_bottomup, conserve = T)

AIC(algae_sem, algae_sem_dsep, algae_sem_topdown, algae_sem_topdown_dsep, algae_sem_bottomup)
