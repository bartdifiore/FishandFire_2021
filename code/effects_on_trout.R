# Environmental effects on trout

source("code/libraries_functions.R")

df <- read.csv("data/raw/raw_220315.csv") %>% janitor::clean_names() %>%
  filter(burn_debris %in% c(0), month == 6) %>%
  mutate(across(all_of(forround), round, 0)) 

drivers <- df[, names(df) %in% c("temp", "do", "avg_max_depth", "avg_daily_disch_nr_nrst_gage", "avg_canopy_cover", "conductivity") == T]

psych::pairs.panels(drivers)



library(glmmTMB)

m1 <- glmmTMB::glmmTMB(trout ~ scale(avg_max_depth) + scale(temp) + scale(do) + scale(avg_canopy_cover) + scale(conductivity) + scale(avg_daily_disch_nr_nrst_gage) + (1|code) + (1|year), df, family = "binomial")
summary(m1)
performance::check_collinearity(m1)


p1 <- tidy(m1, conf.int = T) %>% 
  filter(effect == "fixed") %>%
  mutate(sig = ifelse(p.value < 0.05, "significant", "NS")) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                          term == "scale(avg_max_depth)" ~ "Avg. max. depth", 
                          term == "scale(temp)" ~ "Temp", 
                          term == "scale(do)" ~ "DO", 
                          term == "scale(avg_canopy_cover)" ~ "Canopy", 
                          term == "scale(conductivity)" ~ "Conductivity", 
                          term == "scale(avg_daily_disch_nr_nrst_gage)" ~ "Discharge")) %>%
  filter(term != "Intercept") %>%
  ggplot(aes(x = term, y = estimate))+
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high, fill = sig), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  labs(x = "", y = "Estimate") +
  theme_classic()

vec <- seq(min(df$avg_max_depth), max(df$avg_max_depth), length.out = 100)
pred_trout <- as.data.frame(ggpredict(m1, terms = c("avg_max_depth[vec]"))) %>%
  rename(avg_max_depth = x)

p2 <- ggplot(df, aes(x = avg_max_depth, y = trout))+
  geom_point()+
  geom_line(data = pred_trout, aes(x = avg_max_depth, y = predicted))+
  geom_ribbon(data = pred_trout, aes(x = avg_max_depth, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.25)+
  theme_classic()+
  labs(x = "Average max depth (units)", y = "Trout presence")

cowplot::plot_grid(p1, p2, rel_widths = c(0.5, 1))

ggsave("figures/effects_on_trout.png", device = "png")
