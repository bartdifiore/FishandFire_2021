#------------------------------------------------------------------
## Clean data
#------------------------------------------------------------------

source("code/libraries_functions.R")

raw <- read.csv("data/raw/raw_220113.csv") %>% janitor::clean_names()

responses <- list(physical_chemical = c("avg_max_depth", "temp", "do", "avg_canopy_cover"), food_resources = c("leaf_cpom_wet_wt", "algae_cpom_wet_wt", "nitella_aquatic_veg_cpom_wet_wt"), invert_taxa = c("total_inverts", "total_non_insects", "total_insects", "total_amphibians", "mollusks", "ept", "och", "megaloptera", "rarified_taxa", "pielou_s_j"), invert_traits = c("sensitive", "tolerant", "coll_gath", "coll_filt", "filter_lent", "grazer", "predator", "shredder", "multi", "fast_seas", "small", "large", "cold_steno", "cool_warm_eury", "warm_eury", "pool", "mixed", "riffle", "no_wings", "weak_flght", "strong_flght", "abund_drift", "atmospheric"), invert_indices = c("biotic_index", "thermal_index"))


predictors <- c("code", "month", "year","trout", "burn_debris", "avg_daily_disch_nr_nrst_gage", "max_disch_nr_nrst_gage", "cumulative_junes_dry", "preceding_yr_dry_duration_ys", "yrs_since_disturbance", "rip_m_h_burn", "rip_burn", "basin_m_h_burn", "basin_burn")

meta_names <- c("stream", "code", "latitude", "longitude", "elevation")


for_filter <- c( predictors, unlist(responses, use.names = F))

meta <- raw %>% select(all_of(meta_names)) %>% distinct()

write.csv(meta, "data/derived/metadata.csv", quote = F, row.names = F)

for_round <- for_filter[!for_filter %in% c(predictors, responses$physical_chemical, responses$invert_indices, responses$food_resources, "rarified_taxa", "pielou_s_j")]

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
  mutate(across(all_of(for_round), round, 0), 
         across(all_of(for_round), as.integer))



write.csv(df, "data/derived/cleaned_data.csv", quote = F, row.names = F)
