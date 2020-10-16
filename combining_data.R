# Libraries used

# Defining environment where data is located
setwd("./Data")

# ----------------------------------------------------
# ----------------------------------------------------

# Loading data
tweet_data <- read.csv("processed_tweet_data.csv")
tweet_data <- tweet_data[, -1]

covid_data <- read.csv("processed_covid_data.csv")

# Reformatting dates in datasets
tweet_data$date <- format(as.Date(tweet_data$date), "%m/%d/%Y")
covid_data$X <- format(as.Date(covid_data$X, format = "%m/%d/%Y"), "%m/%d/%Y")

# Saving date vector
dates <- covid_data$X

# Combining the datasets & matching the starting date.
# First, subsetting the tweet_data
tweet_data_subset <- tweet_data[which(tweet_data$date %in% covid_data$X), ]

# Removing date attributes from both:
tweet_data_subset <- tweet_data_subset[, -1]
covid_data <- covid_data[, -1]

# Combining the sets:
combined_data <- cbind(covid_data, tweet_data_subset)
rownames(combined_data) <- dates

# Exporting the data:
write.csv(combined_data, file = "combined_processed_data.csv")
