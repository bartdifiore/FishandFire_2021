source("code/libraries_functions.R")
source("code/clean_data.R")
source("code/theme.R")
source("code/trout_funcitons.R")


df <- read.csv("data/derived/cleaned_trout.csv") %>%
  rename(discharge = avg_daily_disch_nr_nrst_gage) %>%
  mutate(log.discharge = log(discharge), 
         year = as.factor(year))


# General form of the trout model
# y ~ trout * scale(log.discharge) + (1|code) + (1|year)

# Run model selection for each response

out <- list()
for(i in t.responses){
  out[[i]] <- fit_trout(i, df)
}

labels <- c("Total invertebrate abundance", 
            "Rarified taxonomic richness", 
            "Biotic index", 
            "Thermal index", 
            "Total_amphibians", 
            "OCH", 
            "Trichoptera abundance", 
            "Ephemeroptera abundance", 
            "Collector-gatherer abundance", 
            "Collector-filterer abundance", 
            "Filterer-lent abundance", 
            "Grazer abundance", 
            "Predator abundance", 
            "Shredder abundance")

# Build the plots for each response
for(i in 1:length(t.responses)){
  temp <- plot_trout(t.responses[i], out, labels[i])
  ggsave(paste("figures/", labels[i], ".png", sep = ""), temp)
  
}

plot_trout("total_inverts", out, "Total invertebrate abundance")  

ggplot(df, aes(x = trout, y = total_inverts))+
  geom_point(aes(color = trout))

mod.temp <- glmer(coll_gath ~ trout * scale(log.discharge) + (1|code) + (1|year), df, family = poisson(link = "log"))
summary(mod.temp)
car::qqPlot(residuals(mod.temp))
plot(ggpredict(mod.temp, terms = ~ log.discharge*trout))
