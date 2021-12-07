source("code/libraries_functions.R")
source("code/clean_data.R")

#------------------------------------------------------------
## Model univariate relationships
#------------------------------------------------------------

# Here, we model the effect of trout presence, burn status, and drought on invertebrate community structure, including physical characteristics of the community, the abundance of invertebrate taxa, the abundance of invertebrate trait categories, and overall invertebrate indices. We will assume the same set of predictors for each model, and vary the response of interest.

# For simplicity, I'll divide the models and figures into the different "classes" of repsonse variable according to the table of response variables in `Fire-trout hypotheses.docx`


# Build a function for the model

q.glmer <- quietly(glmer)
q.lmer <- quietly(lmer)

predictor.form = c()


mod.function <- function(y, data){
    mod.form <- formula(paste(y, predictor.form))
    mod.temp = q.glmer(mod.form, data, family = "poisson")
    mod.df = tidy(mod.temp$result) %>% 
      mutate(response = y, .before = effect) %>% 
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
    mod.df
  } else{
    mod.temp = q.lmer(form, data)
    mod.df = tidy(mod.temp$result) %>% 
      mutate(response = y, .before = effect) %>% 
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
    mod.df
  }
}


# test it 
mod.function(y = "total_insects", data = df) # works! 
tidy(mod.sample, effects = "fixed") %>% mutate(response = "total_insects", .before = effect)
mod.function(y = "temp", data = df) # works!

mod.predict <- function(y, data){
  response = as.name(y)
  form = substitute(response ~ trout + burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + scale(yrs_since_disturbance) + (1|code) + (1|year)) # model form
  if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the response is integer data, but a linear mixed effects model if the response is non-integer
    mod.temp = q.glmer(form, data, family = "poisson")
    ggpredict(mod.temp$result, terms = ~ burn_debris) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group)
  } else{
    mod.temp = q.lmer(form, data)
    ggpredict(mod.temp$result, terms = ~ burn_debris) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group)
  }
}


# test it 
mod.predict(y = "total_insects", data = df) # works! 
ggpredict(mod.sample, terms = ~ burn_debris)

mod.predict(y = "temp", data = df) # works!

#---------------------------------------------------------------
## Apply model to all responses
#---------------------------------------------------------------

model_sum <- map_dfr(unlist(responses[-2], use.names = F), mod.function, data = df) %>%
  mutate(sig = ifelse(p.value < 0.05 & p.value > 0.01, "*",
                      ifelse(p.value < 0.01 & p.value > 0.001, "**",
                             ifelse(p.value <= 0.001, "***", "NS"))))

ms <- model_sum %>% 
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "physical_chemical",
    response %in% responses[[3]] ~ "invert_taxa", 
    response %in% responses[[4]] ~ "invert_traits", 
    response %in% responses[[5]] ~ "invert_indices"
  )) %>% 
  filter(term == "scale(avg_daily_disch_nr_nrst_gage)" | term == "scale(preceding_yr_dry_duration_ys)" | term == "scale(yrs_since_disturbance)") %>% 
  mutate(term = case_when(
    term == "scale(avg_daily_disch_nr_nrst_gage)" ~ "Average daily discharge", 
    term == "scale(preceding_yr_dry_duration_ys)" ~ "Drying index", 
    term == "scale(yrs_since_disturbance)" ~ "Years since last disturbance"
  )) %>%
  mutate(estimate = ifelse(warning == "NO", estimate, NA))


model_predictions <- map_dfr(unlist(responses[-2], use.names = F), mod.predict, data = df)

mp <- model_predictions %>% 
  rename(burn_debris = x) %>%
  select(-group) %>%
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "physical_chemical",
    response %in% responses[[3]] ~ "invert_taxa", 
    response %in% responses[[4]] ~ "invert_traits", 
    response %in% responses[[5]] ~ "invert_indices"
  )) %>%
  mutate(predicted = ifelse(warning == "NO", predicted, NA))

#---------------------------------------------------------------
## Plot it up
#---------------------------------------------------------------


# Conditional effects plots of burn status, holding continuous predictors at their mean, and assuming trout absent. Plots are conditional mean and 95% confidence intervals at the population-level of the random effect. 


mp %>% filter(response_cat == "physical_chemical") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), show.legend = F)+
  coord_flip()+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional mean", x = "Burn status", title = "Physical and chemical reponses")+
  cowplot::theme_cowplot()

ggsave("figures/physical_chemical.png", device = "png")

mp %>% filter(response_cat == "invert_taxa") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), show.legend = F)+
  coord_flip()+
  facet_wrap(~response, scales = "free", ncol = 3) +
  labs(y = "Conditional mean abundance", x = "Burn status", title = "Invertebrate taxonomic reponses")+
  cowplot::theme_cowplot()

ggsave("figures/invert_taxa.png", device = "png")

mp %>% filter(response_cat == "invert_traits") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), show.legend = F)+
  coord_flip()+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional mean abundance", x = "Burn status", title = "Invertebrate trait class reponses")+
  cowplot::theme_cowplot()

ggsave("figures/invert_traits.png", device = "png")

mp %>% filter(response_cat == "invert_indices") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5), show.legend = F)+
  coord_flip()+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional means", x = "Burn status", title = "Invertebrate indices")+
  cowplot::theme_cowplot()

ggsave("figures/invert_indices.png", device = "png")

# Plots for the effect size of the continuous predictors on each response variable. 

ms %>% mutate(sig.col = ifelse(p.value >= 0.05, "NS", "S")) %>%
  group_by(term) %>%
  mutate(response_ordered = forcats::fct_reorder(response, estimate)) %>%
  ggplot(aes(x = response_ordered, y = estimate))+
  geom_hline(yintercept = 0, lty = 5, color = "gray")+
  geom_pointrange(aes(ymax = estimate + std.error, 
                      ymin = estimate - std.error, fill = sig.col), pch = 21, show.legend = F)+
  scale_fill_manual(values = c(alpha("gray", 0), "black")) +
  
  coord_flip()+
  facet_wrap(response_cat ~ term, scales = "free", ncol = 3)+
  labs(x = "", y = "Effect size")+
  cowplot::theme_cowplot()

ggsave("figures/continuous_predictor_effects.png", device = "png", width = 15, height = 20)



# Scrap to quickly build trout figure 

mod.predict.trout <- function(y, data){
  response = as.name(y)
  form = substitute(response ~ trout + burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + scale(yrs_since_disturbance) + (1|code) + (1|year)) # model form
  if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the repsonse is integer data, but a linear mixed effects model if the response is non-integer
    mod.temp = q.glmer(form, data, family = "poisson")
    ggpredict(mod.temp$result, terms = ~ trout) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group)
  } else{
    mod.temp = q.lmer(form, data)
    ggpredict(mod.temp$result, terms = ~ trout) %>% 
      as.data.frame() %>% 
      mutate(response = y, .before = x) %>%
      mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group)
  }
}

ggpredict(mod.sample, terms = ~ trout, condition = c(burn_debris = "un"))

model_predictions_trout <- map_dfr(unlist(responses[-2], use.names = F), mod.predict.trout, data = df)

mt <- model_predictions_trout %>% 
  rename(trout = x) %>%
  select(-group) %>%
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "physical_chemical",
    response %in% responses[[3]] ~ "invert_taxa", 
    response %in% responses[[4]] ~ "invert_traits", 
    response %in% responses[[5]] ~ "invert_indices"
  )) %>%
  filter(warning == "NO")

mt %>%
  ggplot(aes(x = response, y = predicted))+
  geom_pointrange(aes(color = trout, ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.5))+
  coord_flip()+
  facet_wrap(~response_cat, scales = "free")+
  labs(y = "Conditional mean abundance", x = "Response", title = "Trout effects")+
  cowplot::theme_cowplot()

ggsave("figures/trout_effects.png", device = "png", width = 15, height = 12)