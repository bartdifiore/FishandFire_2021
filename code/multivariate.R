library(vegan)

source("code/libraries_functions.R")
source("code/clean_data.R")

df <- df %>% drop_na(burn_debris)

c.mat <- df[, names(df) %in% responses$taxomonic] %>% as.matrix()

meta <- df[, c("code", "year")]

e.mat <- df[, names(df) %in% c("trout", "burn_debris", responses$environmental)]


nmds <- metaMDS(c.mat)

plot(nmds)

summary(nmds)
nmds

mds <- df[, names(df) %in% c("code", "year", "trout", "burn_debris", responses$environmental)]
mds$mds1 <- nmds$points[,1]
mds$mds2 <- nmds$points[,2]

species <- data.frame(species = rownames(nmds$species), mds1 = nmds$species[,1], mds2 = nmds$species[,2])


ggplot(mds, aes(x = mds1, mds2))+
  geom_path(aes(color = as.numeric(year)), lwd = 1.25)+
  geom_point(aes(fill = burn_debris), pch = 21, size = 3)+
  geom_text(data = species, aes(x = mds1, y = mds2, label = as.numeric(as.factor(species))))+
  facet_wrap(~code)+
  labs(color = "Year", fill = "Burn status")+
  cowplot::theme_cowplot()

ggsave("figures/nmds_bysite.png", device = "png", width = 14, height = 12 )

ggplot(mds, aes(x = mds1, mds2))+
  geom_point(aes(fill = burn_debris), pch = 21, size = 3)+
  cowplot::theme_cowplot()
