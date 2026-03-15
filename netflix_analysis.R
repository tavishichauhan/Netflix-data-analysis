# Netflix Data Analysis Project
# Author: Tavishi Chauhan

library(tidyverse)
library(lubridate)

# Load dataset
netflix <- read.csv("netflix_titles.csv", stringsAsFactors = FALSE)

# Convert date column
netflix$date_added <- mdy(netflix$date_added)

# Remove missing values
netflix_clean <- netflix %>%
  filter(!is.na(type))

# Movies vs TV Shows
p1 <- ggplot(netflix_clean, aes(x = type, fill = type)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Movies vs TV Shows on Netflix")

ggsave("graphs/movies_vs_tv.png", plot = p1)

# Content growth
content_year <- netflix_clean %>%
  group_by(release_year) %>%
  summarise(total = n())

p2 <- ggplot(content_year, aes(x = release_year, y = total)) +
  geom_line(color = "red") +
  theme_minimal() +
  labs(title = "Netflix Content Growth")

ggsave("graphs/content_growth.png", plot = p2)

# Top genres
top_genres <- netflix_clean %>%
  separate_rows(listed_in, sep = ", ") %>%
  count(listed_in, sort = TRUE) %>%
  head(10)

p3 <- ggplot(top_genres, aes(x = reorder(listed_in, n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top Netflix Genres")

ggsave("graphs/top_genres.png", plot = p3)

# Rating distribution
p4 <- ggplot(netflix_clean, aes(x = rating, fill = rating)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Rating Distribution")

ggsave("graphs/rating_distribution.png", plot = p4)