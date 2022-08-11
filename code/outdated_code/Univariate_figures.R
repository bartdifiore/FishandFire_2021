source("code/libraries_functions.R")
source("code/clean_data.R")

df %>%
  drop_na(burn_debris) %>% 
  ggplot(aes(x = yrs_since_disturbance, y = avg_canopy_cover))+
  geom_point(aes(shape = burn_debris, color = burn_debris), size = 3)+
  labs(x = "Years since last disturbance", y = "Average canopy cover (%)", shape = "Disturbance type", color = "Disturbance type")+
  cowplot::theme_cowplot()+
  theme(legend.position = c(0.7, 0.2))

ggsave("figures/canopyXtimedisturbance.png", device = "png")


df %>%
  dplyr::select(code, year, burn_debris, biotic_index, thermal_index, shredder, grazer, predator, coll_gath) %>%
  pivot_longer(cols = biotic_index:coll_gath) %>%
  group_by(year, burn_debris, name) %>%
  summarize(mean_value = mean(value), 
            se = sd(value)/n()) %>%
  ggplot(aes(x = year, y = mean_value))+
  geom_line(aes(color = burn_debris))+
  geom_point(aes(color = burn_debris))+
  geom_linerange(aes(color = burn_debris, ymin = mean_value - se, ymax = mean_value + se))+
  facet_wrap(~name, ncol = 2, scales = "free")+
  cowplot::theme_cowplot()
ggsave("figures/timeseries.png", device = "png", width = 8, height = 8*1.66)



