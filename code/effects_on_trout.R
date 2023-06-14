# Environmental effects on trout

source("code/libraries_functions.R")
source("code/theme.R")

library(tidyverse)
df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month %in% c(3, 6))

drivers <- df[, names(df) %in% c("temp", "do", "avg_max_depth", "avg_daily_disch_nr_nrst_gage", "avg_canopy_cover", "conductivity") == T]

png("figures/multipanel_correlations.png", width = 800, height = 800)
psych::pairs.panels(drivers) # print this out for the supplement...
dev.off()

df %>% 
  mutate(trout.fct = ifelse(trout == 1, "present", "absent"))%>%
  select(code, year, trout.fct, temp, do, avg_max_depth, avg_daily_disch_nr_nrst_gage, avg_canopy_cover, conductivity, preceding_yr_dry_duration_ys) %>%
  pivot_longer(cols = temp:preceding_yr_dry_duration_ys)%>%
  ggplot()+
  geom_boxplot(aes(x = trout.fct, y = value))+
  facet_wrap(~name, scales = "free")


library(glmmTMB)
library(DHARMa)
library(ggeffects)


m1 <- glmmTMB::glmmTMB(trout ~ scale(avg_max_depth) + scale(temp) + scale(do) + scale(avg_canopy_cover) + scale(conductivity) + scale(avg_daily_disch_nr_nrst_gage) + (1|code) + (1|year), df, family = "binomial")
summary(m1)
performance::check_collinearity(m1)  # doesn't appear to have multicollinearity issues
model_checks <- DHARMa::simulateResiduals(m1)
plot(model_checks)

m2 <- update(m1, ~. -scale(avg_daily_disch_nr_nrst_gage))
anova(m2, m1)

m3 <- update(m2, ~. -scale(conductivity))
anova(m3, m2)

m4 <- update(m2, ~. -scale(avg_canopy_cover))
anova(m4, m2)

m5 <- update(m2, ~. -scale(do))
anova(m5, m2)

m6 <- update(m2, ~. -scale(temp))
anova(m6, m2)

m7 <- update(m2, ~. -scale(avg_max_depth))
anova(m7, m2)

performance::check_collinearity(m4)  # doesn't appear to have multicollinearity issues
model_checks <- DHARMa::simulateResiduals(m4)
plot(model_checks)
summary(m4)

m.apriori <- glmmTMB::glmmTMB(trout ~ scale(avg_max_depth) + scale(do) + (1|code) + (1|year), df, family = "binomial")
summary(m.apriori)
performance::check_collinearity(m.test)
model_checks <- DHARMa::simulateResiduals(m.test)
plot(model_checks)

# two panel side by side... 

p1 <- broom.mixed::tidy(m.apriori, conf.int = T) %>% 
  filter(effect == "fixed") %>%
  mutate(sig = ifelse(p.value < 0.05, "significant", "NS")) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                          term == "scale(avg_max_depth)" ~ "Avg. max. depth", 
                          term == "scale(do)" ~ "DO")) %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = forcats::fct_reorder(term, estimate), y = estimate))+
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high, fill = sig), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("black", "white"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  labs(x = "", y = "Estimate") +
  theme_bd()


depth.vec <- seq(min(df$avg_max_depth), max(df$avg_max_depth), length.out = 1000)
depth.df <- as.data.frame(ggeffects::ggpredict(m.apriori, terms = c("avg_max_depth [depth.vec]"), type = "fixed"))


do.vec <- seq(min(df$do), max(df$do), length.out = 1000)
do.df <- as.data.frame(ggeffects::ggpredict(m.apriori, terms = c("do [do.vec]"), type = "fixed", condition = c(avg_max_depth = 0.8650))) # fix depth at third quartile... 


p2 <- ggplot(df, aes(x = avg_max_depth, y = trout))+
  geom_point()+
  geom_line(data = depth.df, aes(x = x, y = predicted))+
  geom_ribbon(data = depth.df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.25)+
  theme_bd()+
  labs(x = "Average max. depth (m)", y = "Trout presence")

p3 <- ggplot(df, aes(x = do, y = trout))+
  geom_point()+
  geom_line(data = do.df, aes(x = x, y = predicted))+
  geom_ribbon(data = do.df, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.25)+
  theme_bd()+
  labs(x = "Dissolved oxygen (mg/L)", y = "Trout presence")

cowplot::plot_grid(p2, p3, nrow = 1, labels = "AUTO")

ggsave("figures/effects_on_trout.png", device = "png", width = 8.5, height = 4)
