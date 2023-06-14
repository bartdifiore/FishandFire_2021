
rawt <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0, 1), month %in% c(3, 6))

t.responses <- c("total_inverts", "rarified_taxa","biotic_index", "thermal_index", "total_amphibians", "och", "trichoptera", "ephemerop", "coll_gath", "coll_filt", "filter_lent", "grazer", "predator", "shredder")

t.predictors <- c("code", "month", "year","trout", "avg_daily_disch_nr_nrst_gage")

t.for_filter <- c( t.predictors, t.responses)

t.for_round <- t.for_filter[!t.for_filter %in% c(t.predictors, "rarified_taxa", "biotic_index", "thermal_index")]

df.t <- rawt %>% select(all_of(t.for_filter)) %>%
  filter(month == 6) %>% # filter out the march sampling in 2009.... The reason that I'm doing this is because seasonality might make those observations very different than the summer observations and the purpose of this analysis is to really focus on the long term effects of fire and trout on stream community structure
  mutate(trout = case_when(
    trout == 0 ~ "absent", 
    trout == 1 ~ "present"
  )) %>% 
  mutate(across(all_of(t.for_round), round, 0), 
         across(all_of(t.for_round), as.integer))

df <- df.t %>%
  rename(discharge = avg_daily_disch_nr_nrst_gage) %>%
  mutate(log.discharge = log(discharge), 
         year = as.factor(year)) %>%
  pivot_longer(total_amphibians:shredder) %>%
  mutate(category = as.factor(name), 
         trout = as.factor(trout))

m1 <- glmer(value ~ trout*name + log.discharge + (1|code) + (1|year), df, family = "poisson")
summary(m1)
model_checks <- DHARMa::simulateResiduals(m1)
plot(model_checks)

m.emm <- emmeans::emmeans (m1,  ~ trout | name)
summary(m.emm, type = "response", infer = T)
confint(m.emm)
emmeans::contrast(m.emm, "pairwise", type = "response", infer = T) %>%
  broom::tidy(conf.int = T) %>%
  ggplot()+
  geom_point(aes(x = name, y = ratio))+
  geom_errorbar(aes(x = name, y = ratio, ymin = conf.low, ymax = conf.high))+
  coord_flip()+
  geom_hline(yintercept = 1)

summary(m.emm, ratios = F, infer = T)

predictions <- as.data.frame(ggpredict(m1, terms = ~trout*name, type = "fixed"))

predictions <- as.data.frame(ggemmeans(m1, terms = ~trout*name, type = "response", infer = T))

predictions %>% 
  select(x, predicted, group, conf.low, conf.high) %>%
  pivot_wider(names_from = x, values_from = c(predicted, conf.low, conf.high)) %>%
  mutate(diff = present - absent) %>%
  ggplot()+
  geom_point(aes(x = group, y = diff))+
  coord_flip()+
  geom_hline(yintercept = 0)
