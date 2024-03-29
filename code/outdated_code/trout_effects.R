source("code/libraries_functions.R")
source("code/clean_data.R")


# Subset the data to only unburned streams

t <- df %>% filter(burn_debris == "un")

# General form of the trout model
    # y ~ trout + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + (1|code) + (1|year)


mod.sample <- glmer(pool ~ trout + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + (1|code) + (1|year), t, family = "poisson")
summary(mod.sample)
plot(ggpredict(mod.sample, ~trout))
car::qqPlot(residuals(mod.sample))

# We want to model the effects of trout on the functional groups, traits, and taxonomic categories. BUT we want to model the effects of the environmental variables on trout.

q.glmer <- quietly(glmer)
q.lmer <- quietly(lmer)

trout_effects <- function(y, data){
  response = as.name(y)
  form = substitute(response ~ trout + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + (1|code) + (1|year)) # model form
  if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the response is integer data, but a linear mixed effects model if the response is non-integer
    
    # run the model
    mod.temp = q.glmer(form, data, family = "poisson")
    
    # run the model predictions and join the posthoc comparisons
    ggpredict(mod.temp$result, terms = ~ trout) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
      mutate(pvalue = round(as.numeric(summary(mod.temp$result)$coefficients[1:2,4]),8))
  } else{
    
    # run the model
    mod.temp = q.lmer(form, data)
    
    # run the model predictions and join the posthoc comparisons
    ggpredict(mod.temp$result, terms = ~ trout) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
      mutate(pvalue = round(as.numeric(summary(mod.temp$result)$coefficients[1:2,5]), 8))
  }
}


trout_effects(y = "pool", t)

model_predictions_trout <- map_dfr(unlist(responses[-1], use.names = F), trout_effects, data = t)

mt <- model_predictions_trout %>% 
  rename(trout = x) %>%
  dplyr::select(-group) %>%
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "Environmental",
    response %in% responses[[2]] ~ "Indices", 
    response %in% responses[[3]] ~ "Taxonomic", 
    response %in% responses[[4]] ~ "Functional groups", 
    response %in% responses[[5]] ~ "Traits"
  )) %>% 
  mutate(predicted = ifelse(warning == "NO", predicted, NA)) %>%
  group_by(response_cat, response) %>%
  mutate(across(.cols = c(predicted, conf.low, conf.high), ~. - first(.))) %>%
  filter(trout != "absent")

mt %>%
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  ungroup() %>%
  group_by(response_cat) %>%
  mutate(response = forcats::fct_reorder(response, predicted)) %>%
  ggplot(aes(x = response, y = predicted))+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, fill = sig),
                  position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  geom_hline(yintercept = 0, linetype = 4)+
  coord_flip()+
  facet_wrap(~response_cat, scales = "free")+
  labs(y = "Conditional mean effect of trout relative to unburnt streams without trout", x = "Response", title = "Trout effects", fill= "Sig. (Absent reference)")+
  cowplot::theme_cowplot()

ggsave("figures/trout_effects.png", device = "png", width = 15, height = 12)


# Environmental effects on trout

drivers <- t[, names(t) %in% c(responses[[1]], "avg_daily_disch_nr_nrst_gage", "preceding_yr_dry_duration_ys") == T]

psych::cor.plot(drivers) # lots of colinearity

png("figures/correlogram.png")
psych::cor.plot(drivers)
dev.off()

psych::pairs.panels(select(drivers, -c(flow, leaf_cpom_wet_wt)))


t <- t %>% 
  mutate(trout.binomial = ifelse(trout == "absent", 0, 1))

#trout presence and depth, temperature, DO, and canopy.

lm1 <- lm(avg_canopy_cover ~ temp, t)
t$res_canopy <- residuals(lm1) # The effects of canopy cover after accounting for temp.

lm2 <- lm(conductivity ~ temp + do, t)
summary(lm2)
t$res_conductivity <- residuals(lm2) # The effects of conductivity after accounting for temp. 

lm3 <- lm(avg_daily_disch_nr_nrst_gage ~ do, t)
summary(lm3)
t$res_discharge <- residuals(lm3)


psych::pairs.panels(t[names(t) %in% c("avg_max_depth", "temp", "do", "res_canopy", "res_conductivity", "res_discharge") == T])

library(glmmTMB)

effects_on_trout <- glmmTMB::glmmTMB(trout.binomial ~ scale(avg_max_depth) + scale(temp) + scale(do) + scale(res_canopy) + scale(res_conductivity) + scale(res_discharge) + (1|code) + (1|year), t, family = "binomial")
summary(effects_on_trout)

p1 <- tidy(effects_on_trout, conf.int = T) %>% 
  filter(effect == "fixed") %>%
  mutate(sig = ifelse(p.value < 0.05, "significant", "NS")) %>%
  mutate(term = case_when(term == "(Intercept)" ~ "Intercept", 
                   term == "scale(avg_max_depth)" ~ "Avg. max. depth", 
                   term == "scale(temp)" ~ "Temp", 
                   term == "scale(do)" ~ "DO", 
                   term == "scale(res_canopy)" ~ "Canopy (res. temp)", 
                   term == "scale(res_conductivity)" ~ "Conductivity (res. temp + do)", 
                   term == "scale(res_discharge)" ~ "Discharge (res. do)")) %>%
  ggplot(aes(x = term, y = estimate))+
  geom_pointrange(aes(y = estimate, ymin = conf.low, ymax = conf.high, fill = sig), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  labs(x = "", y = "Estimate") +
  theme_classic()

vec <- seq(min(t$avg_max_depth), max(t$avg_max_depth), length.out = 100)
pred_trout <- as.data.frame(ggpredict(effects_on_trout, terms = c("avg_max_depth[vec]"))) %>%
  rename(avg_max_depth = x)

p2 <- ggplot(t, aes(x = avg_max_depth, y = trout.binomial))+
  geom_point()+
  geom_line(data = pred_trout, aes(x = avg_max_depth, y = predicted))+
  geom_ribbon(data = pred_trout, aes(x = avg_max_depth, y = predicted, ymin = conf.low, ymax = conf.high), alpha = 0.25)+
  theme_classic()+
  labs(x = "Average max depth (units)", y = "Trout presence")

cowplot::plot_grid(p1, p2, rel_widths = c(0.5, 1))

ggsave("figures/effects_on_trout.png", device = "png")










mod.sample <- glmer(pool ~ trout * scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + (1|code) + (1|year), t, family = "poisson")
summary(mod.sample)

plot(ggpredict(mod.sample, terms = ~avg_daily_disch_nr_nrst_gage*trout))











