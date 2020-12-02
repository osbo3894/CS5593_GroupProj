# Libraries used

# Defining location of data
script_location <- getwd()
data_location <- "./Data Outputs/"
setwd(data_location)

# ----------------------------------------------------
# ----------------------------------------------------

# Loading data
# We're using check.names = false to avoid changing whitespace between names into dots
# (e.g. District of Columbia into District.of.Columbia)
tweet_data <- read.csv("processed_tweet_data.csv", check.names = FALSE)
tweet_data <- tweet_data[, -1]
covid_data <- read.csv("processed_covid_data.csv", check.names = FALSE)

# Reformatting dates in datasets
tweet_data$date <- format(as.Date(tweet_data$date), "%m/%d/%Y")
covid_data[, 1] <- format(as.Date(covid_data[, 1], format = "%m/%d/%Y"), "%m/%d/%Y")

# Saving date vector
dates <- covid_data[, 1]

# Combining the datasets & matching the starting date.
# First, subsetting the tweet_data
tweet_data_subset <- tweet_data[which(tweet_data$date %in% covid_data[, 1]), ]

## Merge the data
colnames(covid_data)[1] <- "date"
combined_data <- merge(covid_data, tweet_data_subset, by="date")

# Exporting the data:
write.csv(combined_data, file = "combined_processed_data.csv", row.names = F, quote=F)
