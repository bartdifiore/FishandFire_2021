source("code/libraries_functions.R")

df <- read.csv("data/raw/hypothesistable.csv")

df %>% group_by(Response_cat) %>%
  pivot_longer(cols = c(BRI:Drying_Index), names_to = "Predictor") %>%
  mutate(value = as.factor(value)) %>%
  ggplot(aes(x = Predictor, y = Response))+
  geom_tile(aes(fill = value), color = "gray20")+
  scale_fill_manual(values = c("red", "white", "blue"))





