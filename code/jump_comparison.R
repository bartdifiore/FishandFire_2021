library(tidyverse)
library(lme4)
library(lmerTest)

# Compare R-output with Jump Pro for Scott


# model structure: 
  # total inverts ~ YrMo * trout + (1|code), family = gaussian...

df <- read.csv("data/raw/raw_firstyear.csv") %>%
  janitor::clean_names() %>%
  select(total_inverts, yr_mo, code, trout) %>%
  as_tibble() %>%
  mutate(yr_mo = as.factor(case_when(yr_mo == 200812 ~ "A-200812", 
                           yr_mo == 20093 ~ "B-20093", 
                           yr_mo == 20096 ~ "C-20096", 
                           yr_mo == 20099 ~ "D-20099", 
                           yr_mo == 20106 ~ "E-20106")),
         trout = as.factor((case_when(trout == 1 ~ "A-present", 
                                      trout == 0 ~ "B-absent"))))


mod1 <- lmer(total_inverts ~ yr_mo*trout + (1|code), data = df, REML = T) 
summary(mod1)

table <- broom.mixed::tidy(mod1) %>%
  mutate(p.value = round(p.value, 3))

write_csv(table, "data/derived/jump_comparison.csv")

MuMIn::r.squaredGLMM(mod1)
