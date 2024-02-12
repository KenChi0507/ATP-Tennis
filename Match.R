library(sparklyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

sc <- spark_connect(master = "local")
df <- spark_read_csv(sc, name ="my_data", path ="C:/Users/FI_0616/Desktop/Dataset/atp_matches.csv",
                     header = TRUE, delimiter = ",")

glimpse(df)
head(df)
sdf_nrow(df)

summy <- df %>%
  select(winner_age, winner_ht, loser_age, loser_ht, tourney_date, tourney_level, surface) %>%
  filter(tourney_date > 20020000, tourney_level != "D") %>%
  sdf_describe()

print(summy)

summary_table <- as.data.frame(summy) 
knitr::kable(summary_table)  

################################################################################
winner_age_hist <- df %>%
  filter(tourney_date > 20020000, tourney_level != "D") %>%
  ggplot() +
  aes(x = winner_age) +
  geom_histogram(fill = "green", bins = 10) +
  labs(x = "Winner Age", y = "Count", title = "Distribution of Winner Ages") +
  theme(panel.border = element_rect(color = "black", fill = NA))

print(winner_age_hist)
################################################################################
loser_age_hist <- df %>%
  filter(tourney_date > 20020000, tourney_level != "D") %>%
  ggplot() +
  aes(x = loser_age) +
  geom_histogram(fill = "red", bins = 10) +
  labs(x = "Loser Age", y = "Count", title = "Distribution of Loser Ages") +
  theme(panel.border = element_rect(color = "black", fill = NA))

print(loser_age_hist)
################################################################################
################################################################################
winner_ht_hist <- df %>%
  filter(tourney_date > 20020000, tourney_level != "D") %>%
  ggplot() +
  aes(x = winner_ht) +
  geom_histogram(fill = "green", bins = 10) +
  labs(x = "Winner Height", y = "Count", title = "Distribution of Winner Height") +
  theme(panel.border = element_rect(color = "black", fill = NA))

print(winner_ht_hist)
################################################################################
loser_ht_hist <- df %>%
  filter(tourney_date > 20020000, tourney_level != "D") %>%
  ggplot() +
  aes(x = loser_ht) +
  geom_histogram(fill = "red", bins = 10) +
  labs(x = "Loser Height", y = "Count", title = "Distribution of Loser Height") +
  theme(panel.border = element_rect(color = "black", fill = NA))

print(loser_ht_hist)
################################################################################
plot_data <- df %>%
  filter(!is.na(surface)) %>%
  count(surface) %>%
  collect()

custom_colors <- c("Grass" = "green", "Clay" = "red", "Hard" = "lightblue", "Carpet" = "orange")

ggplot(plot_data, aes(x = surface, y = n, fill = surface)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, color = "black", fontface = "bold") + 
  scale_fill_manual(values = custom_colors) +
  xlab("Surface") +
  ylab("Count") +
  ggtitle("Surface Distribution") +
  coord_flip()
################################################################################


