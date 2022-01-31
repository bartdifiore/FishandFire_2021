#------------------------------------------------------------------
## Clean data
#------------------------------------------------------------------

source("code/libraries_functions.R")

raw <- read.csv("data/raw/raw_220113.csv") %>% janitor::clean_names()

responses <- list(environmental = c("avg_max_depth", "flow", "temp", "do", "avg_canopy_cover","conductivity", "leaf_cpom_wet_wt"),
                  total_indices = c("total_inverts", "total_amphibians", "rarified_taxa", "pielou_s_j","biotic_index", "thermal_index"),
                  taxomonic = c("ept", "och", "megaloptera",  "ephemerop", "trichoptera", "diptera", "turbellaria", "mollusks"),
                  functional_groups = c("coll_gath", "coll_filt", "filter_lent", "grazer", "predator", "shredder"),
                  traits = c("semi", "uni", "multi", "fast_seas", "non_seas", "small", "medium", "large", "pool","mixed", "riffle", "no_wings", "weak_flght", "strong_flght", "rare_drift", "occ_com_drift", "abund_drift", "abund_drift", "cut_resp", "gills", "atmospheric"))


predictors <- c("code", "month", "year","trout", "burn_debris", "avg_daily_disch_nr_nrst_gage", "preceding_yr_dry_duration_ys", "yrs_since_disturbance")

meta_names <- c("stream", "code", "latitude", "longitude", "elevation")


for_filter <- c( predictors, unlist(responses, use.names = F))

meta <- raw %>% select(all_of(meta_names)) %>% distinct()

write.csv(meta, "data/derived/metadata.csv", quote = F, row.names = F)

for_round <- for_filter[!for_filter %in% c(predictors, responses$environmental, "rarified_taxa", "pielou_s_j", "biotic_index", "thermal_index")]

df <- raw %>% select(all_of(for_filter)) %>%
  filter(month == 6) %>% # filter out the march sampling in 2009.... The reason that I'm doing this is because seasonality might make those observations very different than the summer observations and the purpose of this analysis is to really focus on the long term effects of fire and trout on stream community structure
  mutate(burn_debris = case_when(
           burn_debris == 0 ~ "un", 
           burn_debris == 1 ~ "bri", 
           burn_debris == 2 ~ "brb", 
           burn_debris == 3 ~ "bdf"
         ), 
         trout = case_when(
           trout == 1 ~ "present", 
           trout == 0 ~ "absent"
         )) %>% 
  mutate(conductivity = as.numeric(conductivity), 
         across(all_of(for_round), round, 0), 
         across(all_of(for_round), as.integer))



write.csv(df, "data/derived/cleaned_data.csv", quote = F, row.names = F)
