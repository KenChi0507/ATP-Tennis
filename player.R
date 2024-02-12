library(sparklyr)
library(dplyr)
library(ggplot2)

sc <- spark_connect(master = "local")
df <- spark_read_csv(sc, name ="my_data", path ="C:/Users/FI_0616/Desktop/Dataset/atp_players.csv",
                       header = TRUE, delimiter = ",")
options(digits = 2)

glimpse(df)
head(df)
sdf_nrow(df)

summy <- df %>%
  select(hand, dob, ioc, height) %>%
  filter(dob > 19800000) %>%
  sdf_describe()

print(summy)

summary_table <- as.data.frame(summy) 
knitr::kable(summary_table) 
################################################################################
filtered_df <- df %>%
  select(ioc, dob) %>%
  filter(dob > 19800000)

country_counts <- filtered_df %>%
  group_by(ioc) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  head(20)

ggplot(country_counts, aes(x = reorder(ioc, n), y = n, fill = ioc)) +
  geom_bar(stat = "identity") +
  labs(x = "Country (IOC)", y = "Count") +
  scale_fill_discrete() +
  ggtitle("Top 10 Countries Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
################################################################################
hand_counts <- df %>%
  filter(dob > 19800000, hand %in% c("R", "L")) %>%
  group_by(hand) %>%
  count() %>%
  ungroup()

ggplot(hand_counts, aes(x = n, y = reorder(hand, n))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Count", y = "Hand") +
  ggtitle("Hand Distribution") +
  theme_minimal()