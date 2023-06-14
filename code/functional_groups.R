
df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0, 1), month %in% c(3, 6))

fg <- c("filter_lent", "shredder", "coll_gath", "coll_filt", "grazer", "predator")

t.predictors <- c("code", "month", "year","trout", "avg_daily_disch_nr_nrst_gage", "preceding_yr_dry_duration_ys")

t.for_filter <- c( t.predictors, fg)

# t.for_round <- t.for_filter[!t.for_filter %in% c(t.predictors, "rarified_taxa", "biotic_index", "thermal_index")]

df <- df %>% 
  as_tibble() %>%
  select(all_of(t.for_filter)) %>%
  mutate(trout = case_when(
    trout == 0 ~ "absent", 
    trout == 1 ~ "present"
  )) %>% 
  mutate(across(all_of(fg), round, 0), 
         across(all_of(fg), as.integer)) %>%
  rename(discharge = avg_daily_disch_nr_nrst_gage) %>%
  mutate(log.discharge = log(discharge), 
         year = as.factor(year)) %>%
  pivot_longer(filter_lent:predator) %>%
  mutate(category = as.factor(name), 
         trout = as.factor(trout))

m1 <- glmmTMB(value ~ trout*category + log.discharge + preceding_yr_dry_duration_ys + (1|code) + (1|year), df, family = "poisson")
summary(m1)
model_checks <- DHARMa::simulateResiduals(m1)
plot(model_checks)

m2 <- glmmTMB(value ~ trout*category + log.discharge + preceding_yr_dry_duration_ys + (1|code) + (1|year), df, family = nbinom2(link = "log"))
summary(m2)
model_checks <- DHARMa::simulateResiduals(m2, re.form = NULL)
plot(model_checks)
testResiduals(model_checks)


m3 <- glmmTMB(value ~ trout*category + log.discharge + preceding_yr_dry_duration_ys + (1|code) + (1|year), df, family = nbinom1(link = "log"))
summary(m3)
model_checks <- DHARMa::simulateResiduals(m3)
plot(model_checks)

AIC(m1, m2, m3)

m.emm <- emmeans::emmeans (m2,  ~ trout | category)
summary(m.emm, type = "response", infer = T)
confint(m.emm)


p1 <- emmeans::contrast(m.emm, "pairwise", type = "response", infer = T, adjust = "bonferroni") %>%
  broom::tidy(conf.int = T) %>%
  mutate(p.fct = as.factor(ifelse(p.value >=0.05, "NS", "significant"))) %>%
  ggplot(aes(x = forcats::fct_reorder(category, ratio), y = ratio))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high))+
  geom_point(aes(fill = p.fct), pch = 21, size = 2, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 1, linetype = 4)+
  labs(x = "Functional group", y = "Log ratio\n(trout absent / trout present)")+
  theme_bd()


ggsave("figures/functionalgroups_glmer_contrasts.png", width = 6, height = 4)
