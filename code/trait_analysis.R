
df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month %in% c(3, 6))

traits <- c("gills", "uni", "occ_com_drift", "small", "non_seas", "riffle", "medium", "no_wings", "cut_resp", "abund_drift", "semi", "mixed", "weak_flght", "strong_flght", "multi", "atmospheric", "large", "pool", "fast_seas", "rare_drift")

t.predictors <- c("code", "month", "year","trout", "avg_daily_disch_nr_nrst_gage", "preceding_yr_dry_duration_ys")

t.for_filter <- c( t.predictors, traits)

# t.for_round <- t.for_filter[!t.for_filter %in% c(t.predictors, "rarified_taxa", "biotic_index", "thermal_index")]

df <- df %>% 
  as_tibble() %>%
  select(all_of(t.for_filter)) %>%
  mutate(trout = case_when(
    trout == 0 ~ "B_absent", 
    trout == 1 ~ "A_present"
  )) %>% 
  mutate(across(all_of(traits), round, 0), 
         across(all_of(traits), as.integer)) %>%
  rename(discharge = avg_daily_disch_nr_nrst_gage) %>%
  mutate(log.discharge = log(discharge), 
         year = as.factor(year)) %>%
  pivot_longer(gills:rare_drift) %>%
  mutate(category = as.factor(name), 
         trout = as.factor(trout))

m1 <- glmer(value ~ trout*category + log.discharge + preceding_yr_dry_duration_ys + (1|code) + (1|year), df, family = "poisson")
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



m.emm <- emmeans::emmeans (m2,  ~ trout | category)
summary(m.emm, type = "response", infer = T)
confint(m.emm)
p2 <- emmeans::contrast(m.emm, "pairwise", type = "response", infer = T, adjust = "bonferroni") %>%
  broom::tidy(conf.int = T) %>%
  mutate(p.fct = ifelse(p.value >=0.05, "NS", "significant")) %>%
  ggplot(aes(x = forcats::fct_reorder(category, ratio), y = ratio))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high))+
  geom_point(aes(fill = p.fct), pch = 21, size = 2, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 1, linetype = 4)+
  labs(x = "Trait category", y = "Log ratio\n(trout present / trout absent)")+
  theme_bd()

ggsave("figures/traits_glmer_contrasts.png")


df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month %in% c(3, 6)) %>%
  select(year, code, biotic_index, thermal_index, trout, avg_daily_disch_nr_nrst_gage, preceding_yr_dry_duration_ys) %>%
  mutate(trout = case_when(
    trout == 0 ~ "B_absent", 
    trout == 1 ~ "A_present"
  )) %>%
  pivot_longer(cols = c(biotic_index, thermal_index))

m.index <- lmer(value ~ trout*name + log(avg_daily_disch_nr_nrst_gage) + preceding_yr_dry_duration_ys + (1|code) + (1|year), df)
summary(m.index)
model_checks <- DHARMa::simulateResiduals(m.index, re.form = NULL)
plot(model_checks)

m.emm <- emmeans::emmeans (m.index,  ~ trout | name)
summary(m.emm, type = "response", infer = T)
confint(m.emm)

p1 <- emmeans::contrast(m.emm, "pairwise", type = "response", infer = T, adjust = "bonferroni") %>%
  broom::tidy(conf.int = T) %>%
  mutate(p.fct = ifelse(p.value >=0.05, "NS", "significant")) %>%
  ggplot(aes(x = name, y = estimate))+
  geom_linerange(aes(ymin = conf.low, ymax = conf.high))+
  geom_point(aes(fill = p.fct), pch = 21, size = 2, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  labs(x = "Trait category", y = "Difference (trout present - trout absent)")+
  theme_bd()


cowplot::plot_grid(p1 + labs(x = ""),p2, nrow = 2, rel_heights = c(0.2, 0.8), align = "v", labels = "AUTO")
ggsave("figures/indices_traits.png", width = 6, height = 10)







