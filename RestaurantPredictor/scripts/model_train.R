# scripts/model_train.R

library(dplyr)
library(randomForest)

# Load cleaned data
zomato <- read.csv("data/zomato_cleaned.csv", stringsAsFactors = FALSE)








# Select features
zomato <- zomato %>%
  select(location, rest_type, cuisines, rate, votes, approx_cost.for.two.people., listed_in.type., listed_in.city., dish_liked)

# Rename cost column for convenience
colnames(zomato)[which(names(zomato) == "approx_cost.for.two.people.")] <- "cost"

# === Reduce high-cardinality categorical features ===

# Top 20 locations
top_locations <- names(sort(table(zomato$location), decreasing = TRUE))[1:20]
zomato$location <- ifelse(zomato$location %in% top_locations, zomato$location, "Other")

# Top 20 rest types
top_rest <- names(sort(table(zomato$rest_type), decreasing = TRUE))[1:20]
zomato$rest_type <- ifelse(zomato$rest_type %in% top_rest, zomato$rest_type, "Other")

# Top 20 cuisines
top_cuisines <- names(sort(table(zomato$cuisines), decreasing = TRUE))[1:20]
zomato$cuisines <- ifelse(zomato$cuisines %in% top_cuisines, zomato$cuisines, "Other")

# Top 50 most liked dishes (target)
top_dishes <- names(sort(table(zomato$dish_liked), decreasing = TRUE))[1:50]
zomato <- zomato[zomato$dish_liked %in% top_dishes, ]

# Convert to factors
zomato$location <- as.factor(zomato$location)
zomato$rest_type <- as.factor(zomato$rest_type)
zomato$cuisines <- as.factor(zomato$cuisines)
zomato$listed_in.type. <- as.factor(zomato$listed_in.type.)
zomato$listed_in.city. <- as.factor(zomato$listed_in.city.)
zomato$dish_liked <- as.factor(zomato$dish_liked)

# Train model
set.seed(123)
model <- randomForest(dish_liked ~ ., data = zomato, ntree = 100)

# Save model
saveRDS(model, "models/food_model.rds")
cat("✅ Model trained and saved to models/food_model.rds\n")







