# Load libraries
library(data.table)
library(dplyr)
library(corrplot)

# Load the data from the CSV file
data <- read.csv("Fastfood_complete_data_weather.csv")

variables <- c("ch_in", "sum_elite_status", "max_friends_count", "num_male", 
               "num_female", "sum_fans", "avg_stars", "review_count", 
               "avg_sentiment_score_review", "business_price", "business_park", 
               "business_wifi", "business_takeout", "business_creditcards", 
               "business_delivery", "PRCP", "SNOW", "SNWD", "TOBS")

# Select the relevant columns from the data
selected_data <- data[, variables, drop = FALSE]

# Calculate the correlation matrix
cor_matrix <- cor(selected_data)

# Open a new plotting window
plot.new()

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", tl.cex = 0.7, tl.srt = 45)

results4 <- (cor_matrix)
