
source("code/libraries_functions.R")
source("code/clean_data.R")

#------------------------------------------------------------
## Model univariate relationships
#------------------------------------------------------------

# Here, we model the effect of trout presence, burn status, and drought on invertebrate community structure, including physical characteristics of the community, the abundance of invertebrate taxa, the abundance of invertebrate trait categories, and overall invertebrate indices. We will assume the same set of predictors for each model, and vary the response of interest.

# For simplicity, I'll divide the models and figures into the different "classes" of repsonse variable according to the table of response variables in `Fire-trout hypotheses.docx`

#-------------------------------------------------------------
## Sample model
#-------------------------------------------------------------
df = df %>% 
  mutate(burn_debris = forcats::fct_relevel(burn_debris, "un", before = "bdf")) %>%
  drop_na(burn_debris)

mod.sample <- glmer(total_inverts ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), df, family = "poisson")
summary(mod.sample)
car::qqPlot(residuals(mod.sample))
hist(residuals(mod.sample), breaks = 30)


library (multcomp)
summary(glht(mod.sample, mcp(burn_debris="Tukey")))
summary(glht(mod.sample, mcp(burn_debris= c("bdf - brb = 0"))))


temp <- summary(glht(mod.sample, mcp(burn_debris = c("bdf - un = 0", 
                                                     "brb - un = 0", 
                                                     "bri - un = 0" ))))


    # This just confirms that we don't have collinearity between the two continuous predictors
    plot(max_disch_nr_nrst_gage ~ preceding_yr_dry_duration_ys, df)
    cor.test(df$max_disch_nr_nrst_gage, df$preceding_yr_dry_duration_ys)

# Build a function for the model
    
q.glmer <- quietly(glmer)
q.lmer <- quietly(lmer)
    
mod.function <- function(y, data){
    response = as.name(y)
    form = substitute(response ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year)) # model form
    if(y %in% c("mollusks", "no_wings", "turbellaria") == T){
      mod.temp = q.glmer(form, data[data$burn_debris != "bdf", ], family = "poisson")
      mod.df = tidy(mod.temp$result) %>% 
        mutate(response = y, .before = effect) %>% 
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = p.value)
      mod.df
    } else {
    if(is.integer(data[,y]) == T){ # this just fits a poisson glmer if the repsonse is integer data, but a linear mixed effects model if the response is non-integer
      mod.temp = q.glmer(form, data, family = "poisson")
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
      }}
}


        # test it 
            mod.function(y = "conductivity", data = df) # works! 
            tidy(mod.sample, effects = "fixed") %>% mutate(response = "total_insects", .before = effect)
            mod.function(y = "temp", data = df) # works!

mod.predict <- function(y, data){
    response = as.name(y)
    
    form = substitute(response ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year)) # model form
    
    if(y %in% c("mollusks", "no_wings", "turbellaria") == T){
      # run the model
      mod.temp = q.glmer(form, data[data$burn_debris != "bdf", ], family = "poisson")
      
      # estimate post hoc comparisons      
      post.hoc = summary(glht(mod.temp$result, mcp(burn_debris=c("brb - un = 0", 
                                                                 "bri - un = 0"))))
      df.post = data.frame(response = y,
                           x = c("brb", "bri"),
                           comparison = names(post.hoc$test$coefficients), 
                           difference = post.hoc$test$coefficients,
                           pvalue = round(post.hoc$test$pvalues,8)
      )
      
      dummy.row <- data.frame(response = y, x = "bdf", predicted = 0, std.error = NA, 
                              conf.low = NA, conf.high = NA, group = as.factor("1"), warning = "NO", comparison = "NA", difference = NA, pvalue = NA)
      # run the model predictions and join the posthoc comparisons
      ggpredict(mod.temp$result, terms = ~ burn_debris) %>% 
        as.data.frame() %>% 
        mutate(response = y, .before = x) %>%
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
        left_join(df.post) %>%
        bind_rows(dummy.row)
      
    } else{
    if(is.integer(data[,y]) == T ){ # this just fits a poisson glmer if the response is integer data, but a linear mixed effects model if the response is non-integer
      
      # run the model
      mod.temp = q.glmer(form, data, family = "poisson")

      # estimate post hoc comparisons      
      post.hoc = summary(glht(mod.temp$result, mcp(burn_debris=c("bdf - un = 0", 
                                                                 "brb - un = 0", 
                                                                 "bri - un = 0"))))
      df.post = data.frame(response = y,
                           x = c("bdf", "brb", "bri"),
                           comparison = names(post.hoc$test$coefficients), 
                           difference = post.hoc$test$coefficients,
                           pvalue = round(post.hoc$test$pvalues,8)
      )
      
      # run the model predictions and join the posthoc comparisons
      ggpredict(mod.temp$result, terms = ~ burn_debris) %>% 
        as.data.frame() %>% 
        mutate(response = y, .before = x) %>%
        mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
        left_join(df.post)
        } else{
          
          # run the model
          mod.temp = q.lmer(form, data)
          
          # estimate post hoc comparisons      
          post.hoc = summary(glht(mod.temp$result, mcp(burn_debris=c("bdf - un = 0", 
                                                                     "brb - un = 0", 
                                                                     "bri - un = 0"))))
          df.post = data.frame(response = y,
                               x = c("bdf", "brb", "bri"),
                               comparison = names(post.hoc$test$coefficients), 
                               difference = post.hoc$test$coefficients,
                               pvalue = round(post.hoc$test$pvalues,8)
          )
          
          ggpredict(mod.temp$result, terms = ~ burn_debris) %>% 
            as.data.frame() %>% 
            mutate(response = y, .before = x) %>%
            mutate(warning = ifelse(length(mod.temp$warnings) == 0, "NO", mod.temp$warnings), .after = group) %>%
            left_join(df.post)
  }}
}


# test it 
mod.predict(y = "conductivity", data = df) # works! 
as.data.frame(ggpredict(mod.sample, terms = ~burn_debris))

#---------------------------------------------------------------
## Apply model to all responses
#---------------------------------------------------------------
            
model_sum <- map_dfr(unlist(responses, use.names = F), mod.function, data = df) %>%
              mutate(sig = ifelse(p.value < 0.05 & p.value > 0.01, "*",
                                  ifelse(p.value < 0.01 & p.value > 0.001, "**",
                                         ifelse(p.value <= 0.001, "***", "NS"))))

ms <- model_sum %>% 
  mutate(response_cat = case_when(
    response %in% responses[[1]] ~ "Environmental",
    response %in% responses[[2]] ~ "Indices", 
    response %in% responses[[3]] ~ "Taxonomic", 
    response %in% responses[[4]] ~ "Functional groups", 
    response %in% responses[[5]] ~ "Traits"
  )) %>% 
  filter(term == "scale(avg_daily_disch_nr_nrst_gage)" | term == "scale(preceding_yr_dry_duration_ys)") %>% 
  mutate(term = case_when(
    term == "scale(avg_daily_disch_nr_nrst_gage)" ~ "Average daily discharge", 
    term == "scale(preceding_yr_dry_duration_ys)" ~ "Drying index"
  )) %>%
  mutate(estimate = ifelse(warning == "NO", estimate, NA))
  

model_predictions <- map_dfr(unlist(responses, use.names = F), mod.predict, data = df)

mp <- model_predictions %>% 
  rename(burn_debris = x) %>%
  dplyr::select(-c(group)) %>%
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
  filter(burn_debris != "un")

#---------------------------------------------------------------
## Plot it up
#---------------------------------------------------------------


# Conditional effects plots of burn status, holding continuous predictors at their mean, and assuming trout absent. Plots are conditional mean and 95% confidence intervals at the population-level of the random effect. 


mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  filter(response_cat == "Environmental") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  geom_hline(yintercept = 0, linetype = 4)+
  coord_flip()+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional mean difference\nfrom unburned streams", x = "Burn status", title = "Environmental reponses")+
  cowplot::theme_cowplot()

ggsave("figures/environmental.png", device = "png")

mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  filter(response_cat == "Indices") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free", ncol = 3) +
  labs(y = "Conditional mean difference\nfrom unburned streams", x = "Burn status", title = "Aggregate reponses")+
  cowplot::theme_cowplot()

ggsave("figures/total_indices.png", device = "png")

mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  filter(response_cat == "Taxonomic") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free", ncol = 3) +
  labs(y = "Conditional mean difference\nfrom unburned streams", x = "Burn status", title = "Taxonomic reponses")+
  cowplot::theme_cowplot()

ggsave("figures/taxonomic.png", device = "png")

mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  filter(response_cat == "Functional groups") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional mean difference\nfrom unburned streams", x = "Burn status", title = "Functional group reponses")+
  cowplot::theme_cowplot()

ggsave("figures/functional_groups.png", device = "png")

mp %>% 
  mutate(sig = ifelse(pvalue < 0.05, "significant", "NS")) %>%
  filter(response_cat == "Traits") %>% 
  ggplot(aes(x = burn_debris, y = predicted))+
  geom_pointrange(aes(color = burn_debris, ymin = conf.low, ymax = conf.high, fill = sig), position = position_dodge(width = 0.5), pch = 21, show.legend = F)+
  scale_fill_manual(values = c("white", "black"))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype = 4)+
  facet_wrap(~response, scales = "free") +
  labs(y = "Conditional mean difference\nfrom unburned streams", x = "Burn status", title = "Trait indices")+
  cowplot::theme_cowplot()

ggsave("figures/trait.png", device = "png")

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
  facet_wrap(response_cat ~ term, scales = "free", ncol = 2)+
  labs(x = "", y = "Effect size")+
  cowplot::theme_cowplot()

ggsave("figures/continuous_predictor_effects.png", device = "png", width = 15, height = 20)





# Problematic models

# Mollusks
  # Mollusks will not converge because there were no observed mollusks in bdf streams. So I've subset the dataframe to exclude bdf streams, and then refit the model. I'll plot bdf as zero for mollusks without confidence intervals in the figure.

df %>% group_by(burn_debris) %>%
  summarize(mollusks = sum(mollusks))

mollusks <- glmer(mollusks ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df[df$burn_debris != "bdf", ], family = "poisson")
summary(mollusks)

mod.predict(y = "mollusks", data = df[df$burn_debris != "bdf", ])

# No wings
  # No wings will not converge because there were no observed no wings in bdf streams. So I've subset the dataframe to exclude bdf streams, and then refit the model. I'll plot bdf as zero for mollusks without confidence intervals in the figure.

df %>% group_by(burn_debris) %>%
  summarize(no_wings = sum(no_wings))

nowings <- glmer(no_wings ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df[df$burn_debris != "bdf", ], family = "poisson")
summary(nowings)

# turbellaria
  # Same issue!!! as mollusks and no wings

df %>% group_by(burn_debris) %>%
  summarize(turbellaria = sum(turbellaria))

turbellaria <- glmer(turbellaria ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df[df$burn_debris != "bdf", ], family = "poisson")
summary(turbellaria)


# filter_lent
  # The issue here is that 50% of the observations are zeros. So I will fit with a zero inflated model.
length(df$filter_lent[df$filter_lent == 0]) / length(df$filter_lent) 

filter <- glmmTMB::glmmTMB(filter_lent ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df, family = "poisson", ziformula = ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys))
summary(filter)

fit <- glmer(filter_lent ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df, family = "poisson")
summary(fit)

AIC(filter, fit)
# algae_cpom_wet_wt

length(df$algae_cpom_wet_wt[df$algae_cpom_wet_wt == 0]) / length(df$algae_cpom_wet_wt) 
# The issue here is that 60% of the observations are zeros. It is also filled with NA's so I don't think any model will converge... dropping it for now. 
algae <- glmmTMB::glmmTMB(algae_cpom_wet_wt ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), data = df, family = "gaussian", ziformula = ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys))
summary(algae)



mod.sample <- lmer(conductivity ~ burn_debris + scale(avg_daily_disch_nr_nrst_gage) + scale(preceding_yr_dry_duration_ys) + trout + (1|code) + (1|year), df)
summary(mod.sample)
car::qqPlot(residuals(mod.sample))
hist(residuals(mod.sample), breaks = 30)

