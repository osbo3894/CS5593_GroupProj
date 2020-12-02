# Libraries used

# Defining environment where script and data are located
script_location <- getwd()
data_location <- "./COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us"
setwd(data_location)

# ----------------------------------------------------
# ----------------------------------------------------

# Collecting .csv filenames from folder
filenames = list.files(pattern="*.csv")

# Vectors of states we are interested in
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
            "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
            "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
            "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
            "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
            "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
            "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
            "West Virginia", "Wisconsin", "Wyoming")

# Defining the date range we are interested in.
# Actual start date is one plus the current date, as that one is set as the benchmark
# to calculate daily increases.

dates <- seq.Date(from = as.Date("04/12/2020", format = "%m/%d/%Y"),
                  to = as.Date(Sys.Date() -1, format = "%m/%d/%Y"), by = "day")
dates <- format(dates, "%m/%d/%Y")

# Creating the dataframe in which the confirmed cases will be stored
covid_data <- as.data.frame(matrix(0, nrow = length(states), ncol = length(dates)))
rownames(covid_data) <- states
colnames(covid_data) <- dates

# Looping over each .csv file
for(i in 1:length(filenames)){
  
  # First, reading the datafile
  data <- read.csv(filenames[i])
  
  # Subsetting the data based on states we're interested in:
  data <- data[which(data[, 1] %in% states), ]
  
  # Extracting "Confirmed cases" and adding it to our dataframe
  covid_data[, i] <- data$Confirmed
  
}

# Subsetting final data & transposing
final_covid_data <- covid_data[, 1:length(dates)]
final_covid_data_transpose <- as.data.frame(t(as.matrix(final_covid_data)))

# Converting to daily increases, rather than cumulative. Setting the base to the the first
# date defined in dates
final_covid_data <- final_covid_data_transpose

for(i in 2:nrow(final_covid_data_transpose)){
  
  final_covid_data[i, ] <- final_covid_data_transpose[i, ] - final_covid_data_transpose[(i - 1), ]
  
}

# Removing the benchmark date
final_covid_data <- final_covid_data[-1, ]

# Exporting final transpose covid data
# First, setting the working directory to be in the data folder
output_location <- paste0(script_location, "/Data Outputs")
setwd(output_location)

# Now exporting
write.csv(final_covid_data, file = "processed_covid_data.csv")
