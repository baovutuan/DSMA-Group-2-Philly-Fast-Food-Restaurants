# Load the data from the CSV file
data <- read.csv("Fastfood_complete_data_weather.csv")

# Specify the variables you're interested in
variables_of_interest <- c("ch_in", "sum_elite_status", "max_friends_count", "num_male", "num_female", "sum_fans", "avg_stars", "review_count", "avg_sentiment_score_review", "business_price", "business_park", "business_wifi", "business_takeout", "business_creditcards", "business_delivery", "PRCP", "SNOW", "SNWD", "TOBS")

# Create a subset of the data with only the specified variables
subset_data <- data[, variables_of_interest]

# Function to replace missing or non-numeric values with the mean of the column
replace_with_mean <- function(x) {
  ifelse(is.na(x) | !is.finite(x), mean(x, na.rm = TRUE), x)
}

# Apply the replacement function to the subset of data
cleaned_data <- sapply(subset_data, replace_with_mean)

# Calculate statistics
summary_stats <- summary(cleaned_data)
mean_stats <- sapply(cleaned_data, mean, na.rm = TRUE)
median_stats <- sapply(cleaned_data, median, na.rm = TRUE)
min_stats <- sapply(cleaned_data, min, na.rm = TRUE)
max_stats <- sapply(cleaned_data, max, na.rm = TRUE)
sd_stats <- sapply(cleaned_data, sd, na.rm = TRUE)

# Combine the results into a data frame
results3 <- data.frame(
  Count = as.numeric(summary_stats[1, ]),
  Mean = mean_stats,
  Median = median_stats,
  Min = min_stats,
  Max = max_stats,
  SD = sd_stats
)