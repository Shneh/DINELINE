# scripts/data_cleaning.R

library(dplyr)
library(stringr)

# Load the dataset
zomato <- read.csv("data/zomato.csv", stringsAsFactors = FALSE)

# Drop unneeded columns
zomato <- zomato %>%
  select(-c(url, address, phone, reviews_list, menu_item))

# Keep only the first liked dish (target variable)
zomato$dish_liked <- sapply(strsplit(as.character(zomato$dish_liked), ","), `[`, 1)

# Drop rows where dish_liked is missing
zomato <- zomato[!is.na(zomato$dish_liked), ]

# Clean rate column (remove "/5", handle "NEW", etc.)
zomato$rate <- str_trim(zomato$rate)
zomato$rate <- ifelse(zomato$rate %in% c("NEW", "-", ""), NA, gsub("/5", "", zomato$rate))
zomato$rate <- as.numeric(zomato$rate)

# Drop rows with NA in important columns
zomato <- zomato %>%
  filter(!is.na(rate) & !is.na(cuisines) & !is.na(approx_cost.for.two.people.))

# Save cleaned dataset
write.csv(zomato, "data/zomato_cleaned.csv", row.names = FALSE)

cat("✅ Data cleaned and saved to data/zomato_cleaned.csv\n")
