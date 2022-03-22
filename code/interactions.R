source("code/clean_data.R")
source("code/fitting_functions.R")

#------------------------------------------------------------
## Model univariate relationships
#------------------------------------------------------------

# Here, we model the effect of trout presence, burn status, and drought on invertebrate community structure, including physical characteristics of the community, the abundance of invertebrate taxa, the abundance of invertebrate trait categories, and overall invertebrate indices. We will assume the same set of predictors for each model, and vary the response of interest.

# For simplicity, I'll divide the models and figures into the different "classes" of repsonse variable according to the table of response variables in `Fire-trout hypotheses.docx`

#-------------------------------------------------------------
## Sample model
#-------------------------------------------------------------

df <- df %>% 
  drop_na(treatment) %>%
  mutate(treatment = as.factor(treatment),
         treatment = forcats::fct_relevel(treatment, "UNTL", "UNT", "BRITL", "BRBTL", "BDFTL")) %>%
  mutate(year = as.factor(year), 
         code = as.factor(code), 
         discharge.log = log(avg_daily_disch_nr_nrst_gage), 
         drying.sqrt = sqrt(preceding_yr_dry_duration_ys)) %>%
  filter(treatment != "BRIT")


mod.sample <- glmer(ephemerop ~ treatment * (scale(discharge.log) + scale(drying.sqrt)) + (1|code) + (1|year), df, family = "poisson")
summary(mod.sample)
pred <- ggpredict(mod.sample,  terms = ~ discharge.log * treatment + drying.sqrt * treatment)
plot(pred)

#-------------------------------------------------------------
## Fit models
#-------------------------------------------------------------

sub <- c(responses[[3]],responses[[2]])

model_sum <- map_dfr(sub, compile_results, data = df) %>%
  mutate(sig = ifelse(p.value < 0.05 & p.value > 0.01, "*",
                      ifelse(p.value < 0.01 & p.value > 0.001, "**",
                             ifelse(p.value <= 0.001, "***", "NS"))))

write.csv(model_sum, file = "data/derived/interactions_summaries.csv", row.names = F)

#-------------------------------------------------------------
## Generate predictions
#-------------------------------------------------------------
predict_glmer(y = "total_inverts", df)

library(multcomp)
model_predictions <- map_dfr(c(responses[[1]],responses[[2]], responses[[3]]), predict_glmer, data = df)

mp <- model_predictions %>% 
  rename(treatment = x) %>%
  dplyr::select(-c(group)) %>%
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "Environmental",
    response %in% responses[[2]] ~ "Indices", 
    response %in% responses[[3]] ~ "Taxonomic", 
  )) %>% 
  mutate(predicted = ifelse(warning == "NO", predicted, NA)) %>%
  group_by(response_cat, response) %>%
  mutate(across(.cols = c(predicted, conf.low, conf.high), ~. - first(.))) %>%
  filter(treatment != "UNTL")


#---------------------------------------------------------------
## Plot it up
#---------------------------------------------------------------


# Conditional effects plots of treatment, holding continuous predictors at their mean, and assuming trout absent. Plots are conditional mean and 95% confidence intervals at the population-level of the random effect. P-values are unreliable as higher order interactions are significant.


mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS"),
         treatment = forcats::fct_rev(treatment)) %>%
  filter(response_cat == "Indices") %>%
  ggplot(aes(x = treatment, y = predicted))+
  geom_pointrange(aes(color = treatment, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free", ncol = 1) +
  labs(y = "Conditional mean difference\nfrom unburned streams w/out trout", x = "Treatment", title = "General indices")+
  cowplot::theme_cowplot()

ggsave("figures/total_indices.png", device = "png", width = 4, height = 8.5)

mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS"),
         treatment = forcats::fct_rev(treatment)) %>%
  filter(response_cat == "Taxonomic") %>%
  ggplot(aes(x = treatment, y = predicted))+
  geom_pointrange(aes(color = treatment, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free", ncol = 1) +
  labs(y = "Conditional mean difference\nfrom unburned streams w/out trout", x = "Treatment", title = "General indices")+
  cowplot::theme_cowplot()

ggsave("figures/taxonomic.png", device = "png", width = 4, height = 8.5)


mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS"),
         treatment = forcats::fct_rev(treatment)) %>%
  filter(treatment != "UNT") %>%
  filter(response_cat == "Environmental") %>%
  ggplot(aes(x = treatment, y = predicted))+
  geom_pointrange(aes(color = treatment, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free", ncol = 1) +
  labs(y = "Conditional mean difference\nfrom unburned streams w/out trout", x = "Treatment", title = "Environmental responses")+
  cowplot::theme_cowplot()

ggsave("figures/environmental.png", device = "png", width = 4, height = 8.5)


#---------------------------------------------------------------------------
## Scrap and checks
#---------------------------------------------------------------------------


mod.temp <- lmer(algae_cpom_wet_wt ~ treatment * (scale(discharge.log) + scale(drying.sqrt)) + (1|code) + (1|year), df)
summary(mod.temp) # Seems to be working now...










