#------------------------------------------------------------------
## Clean trout data
#------------------------------------------------------------------

source("code/libraries_functions.R")

rawt <- read.csv("data/raw/raw_trout.csv") %>% janitor::clean_names()

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



write.csv(df.t, "data/derived/cleaned_trout.csv", quote = F, row.names = F)


#------------------------------------------------------------------
## Clean fire data
#------------------------------------------------------------------

rawf <- read.csv("data/raw/raw_fire.csv") %>% janitor::clean_names()

f.responses <- c("total_inverts", "rarified_taxa","biotic_index", "thermal_index", "total_amphibians", "och", "trichoptera", "ephemerop", "coll_gath", "coll_filt", "filter_lent", "grazer", "predator", "shredder")

f.predictors <- c("code", "month", "year","burn_debris", "avg_daily_disch_nr_nrst_gage", "preceding_yr_dry_duration_ys")

f.for_filter <- c( f.predictors, f.responses)

f.for_round <- f.for_filter[!f.for_filter %in% c(f.predictors, "rarified_taxa", "biotic_index", "thermal_index")]

df.f <- rawf %>% select(all_of(f.for_filter)) %>%
  filter(month == 6) %>% # filter out the march sampling in 2009.... The reason that I'm doing this is because seasonality might make those observations very different than the summer observations and the purpose of this analysis is to really focus on the long term effects of fire and trout on stream community structure
  mutate(trout = case_when(
    burn_debris == 0 ~ "un", 
    burn_debris == 1 ~ "bri", 
    burn_debris == 2 ~ "brb", 
    burn_debris == 3 ~ "bdf"
  )) %>% 
  mutate(across(all_of(f.for_round), round, 0), 
         across(all_of(f.for_round), as.integer))

write.csv(df.f, "data/derived/cleaned_fire.csv", quote = F, row.names = F)
