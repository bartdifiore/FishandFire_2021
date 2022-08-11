
library(piecewiseSEM)

source("code/clean_data.R")

df.t$trout.num <- as.integer(ifelse(df.t$trout == "absent", 0, 1))

basic_sem <- psem(
  glmer(och ~ trout.num + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.t, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout.num + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.t, family = poisson(link = "log")), 
  glmer(grazer ~ trout.num + (1|code) + (1|year), df.t, family = poisson(link = "log"))#,
  # glmer(trout.num ~ avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.t, family = binomial(link = "logit"))
)

summary(basic_sem, conserve = T)
coefs(basic_sem, standardize.type = "latent.linear")
plot(basic_sem)



forround <- c("och", "grazer", "coll_gath", "shredder")

raw <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 



basic_sem <- psem(
  glmer(och ~ trout + avg_max_depth + algae_cpom_wet_wt + (1|code) + (1|year), raw, family = poisson(link = "log")),
  glmer(coll_gath ~ och + avg_max_depth + grazer + trout + algae_cpom_wet_wt + (1|code) + (1|year), raw, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + och + algae_cpom_wet_wt + (1|code) + (1|year), raw, family = poisson(link = "log")),
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), raw, family = binomial(link = "logit"))
)

summary(basic_sem, conserve = T)
coefs(basic_sem, standardize.type = "latent.linear")
plot(basic_sem)


basic_sem2 <- psem(
  glmer(och ~ trout + (1|code) + (1|year), raw, family = poisson(link = "log")),
  glmer(coll_gath ~ och + avg_max_depth + grazer + trout + (1|code) + (1|year), raw, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + (1|code) + (1|year), raw, family = poisson(link = "log")),
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), raw, family = binomial(link = "logit"))
)

summary(basic_sem2, conserve = T)
coefs(basic_sem, standardize.type = "latent.linear")
plot(basic_sem2)








forround <- c("och", "grazer", "coll_gath", "shredder")

raw <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 


df.sem3 <- raw %>%
  select(code, year, trout, och, coll_gath, grazer, shredder, algae_cpom_wet_wt, leaf_cpom_wet_wt, avg_canopy_cover, avg_max_depth)

# This is the top down model hypothesized in the DAG, but I have cut out flow due to sample size issue and the fact that preceeding year dry duration has almost no variation (all zeros).

sem3 <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + avg_max_depth + trout + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(shredder ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  lmer(algae_cpom_wet_wt ~ avg_canopy_cover + coll_gath + grazer + (1|code) + (1|year), df.sem3),
  lmer(leaf_cpom_wet_wt ~ avg_canopy_cover + shredder + (1|code) + (1|year), df.sem3)
)

summary(sem3, conserve = T)
plot(sem3)


#Now I will add paths based on the d-separate tests. I will add paths that are considered significant. If an D-separateion test is p > 0.05 we would fail to reject the model. Therefore if p < 0.05 we reject the model and add the claim back into the model. 

sem4 <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + avg_max_depth + trout + avg_canopy_cover + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + avg_canopy_cover + och + coll_gath + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(shredder ~ trout + avg_max_depth + avg_canopy_cover + coll_gath + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  lmer(algae_cpom_wet_wt ~ avg_canopy_cover + coll_gath + grazer + (1|code) + (1|year), df.sem3),
  lmer(leaf_cpom_wet_wt ~ avg_canopy_cover + shredder + grazer + (1|code) + (1|year), df.sem3)
)

summary(sem4, conserve = T)
plot(sem4)







raw <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(month == 6)





#------------------------------------------------------------


forround <- c("och", "grazer", "coll_gath", "shredder", "algae_cpom_wet_wt", "leaf_cpom_wet_wt")

raw <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 


df.sem3 <- raw %>%
  select(code, year, trout, och, coll_gath, grazer, shredder, algae_cpom_wet_wt, leaf_cpom_wet_wt, avg_canopy_cover, avg_max_depth, flow, avg_daily_disch_nr_nrst_gage)

# This is the top down model hypothesized in the DAG, but I have cut out flow due to sample size issue and the fact that preceeding year dry duration has almost no variation (all zeros).

sem_topdown <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(shredder ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(algae_cpom_wet_wt ~ coll_gath + grazer + och + trout + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(leaf_cpom_wet_wt ~ shredder + (1|code) + (1|year), df.sem3, family = poisson(link = "log"))
)

sem_topdown2 <- update(sem_topdown, och %~~% grazer)
summary(sem_topdown2)
plot(sem_topdown2)




# could not get the bottom up model to fit because of the trout glmer, can get it to run independently but not as part of psem
sem_bottomup <- psem(
  glmer(trout ~ och + algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), 
        data = df.sem3, 
        family = binomial(link = "logit")),
  glmer(och ~ coll_gath + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(shredder ~ leaf_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log"))
)

sem_bottomup2 <- update(sem_bottomup, och %~~% grazer)
summary(sem_bottomup2, conserve = T)
plot(sem_bottomup2)











m1 <- glm(cbcl ~ sex  +  bw_kgs  + sex:bw_kgs,
          data = d,
          family = Gamma(link=identity))

m2 <- glmer(cbcl ~ sex  +  bw_kgs  + sex:bw_kgs +  (1|site_id/family_id),
            data = d,
            family = Gamma(link=identity),
            start=list(fixef=coef(m1)),
            control=glmerControl(nAGQ0initStep=FALSE),
            verbose = 100)

m1 <- glm(trout ~ och + coll_gath + shredder + avg_max_depth, 
          data = df.sem3, 
          family = binomial(link = "logit"))

m2 <- glmer(trout ~ och + coll_gath + shredder + avg_max_depth + (1|code) + (1|year), 
            data = df.sem3, 
            family = binomial(link = "logit"), 
            start = list(fixef = coef(m1)))


temp <- glmer(trout ~ och + coll_gath + shredder + avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit"))








#------------------------------------------
## Scott hypotheses (as of June 13, 2022)
#------------------------------------------

sem_apriori <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + algae_cpom_wet_wt + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + algae_cpom_wet_wt + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(shredder ~ leaf_cpom_wet_wt + flow + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(algae_cpom_wet_wt ~ avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log"))
)

sem_apriori2 <- update(sem_apriori, trout %~~% shredder)
sem_apriori3 <- update(sem_apriori2, och %~~% grazer)

summary(sem_apriori3, conserve = T)
plot(sem_apriori3)



sem_bd <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + algae_cpom_wet_wt + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + algae_cpom_wet_wt + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + algae_cpom_wet_wt + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(algae_cpom_wet_wt ~ trout + avg_daily_disch_nr_nrst_gage + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log"))
)

sem_bd2 <- update(sem_bd, och %~~% grazer)

summary(sem_bd2, conserve = T)
plot(sem_bd2)









forround <- c("och", "grazer", "coll_gath", "shredder")

raw <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(month == 6, year > 2012) %>%
  mutate(across(all_of(forround), round, 0)) 

df <- raw %>%
  select(code, year, burn_debris, trout, och, coll_gath, grazer, shredder, algae_cpom_wet_wt, leaf_cpom_wet_wt, avg_canopy_cover, avg_max_depth, flow, avg_daily_disch_nr_nrst_gage) %>%
  mutate(burn_debris = case_when(
    burn_debris == 0 ~ "un", 
    burn_debris == 1 ~ "bri", 
    burn_debris == 2 ~ "brb", 
    burn_debris == 3 ~ "bdf"
  ))


sem_bd <- psem(
  glmer(trout ~ avg_max_depth + (1|code) + (1|year), df.sem3, family = binomial(link = "logit")),
  glmer(och ~ trout + avg_max_depth + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  glmer(coll_gath ~ och + trout + avg_max_depth + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log")), 
  glmer(grazer ~ trout + avg_max_depth + algae_cpom_wet_wt + avg_daily_disch_nr_nrst_gage + (1|code) + (1|year), df.sem3, family = poisson(link = "log")),
  lmer(algae_cpom_wet_wt ~ avg_daily_disch_nr_nrst_gage + trout + och + avg_max_depth + coll_gath + (1|code) + (1|year), df.sem3, family = poisson(link = "log"))
)

summary(sem_bd, conserve = T)
plot(sem_bd)





