# Libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)
library(reshape)
library(wordcloud)

# Defining work environments
dashboard_location <- getwd()
base_location <- dirname(getwd())
data_location <- paste0(base_location, "/Data Outputs")

# Loading data
setwd(data_location)
data <- read.csv("combined_processed_data.csv", check.names = FALSE)

# Processing data
colnames(data)[1] <- "Date"
data$Date <- as.Date(data$Date, format = "%m/%d/%y")

# Source UI
setwd(dashboard_location)
ui <- source("ui.R")

# ----------------------------------------------------------------------
# Defining backend functionality
server <- function(input, output){
  
  # Plotting Confirmed cases time series
  output$covid_timeline <- renderPlot({

    states <- input$timeline_state
    timeline <- seq(from = input$timeline_date[1], to = input$timeline_date[2],
                    by = "day")
    
    if(length(states) == 1){
      
      # Subsetting data based on dates selected by user
      state_data <- data[data$Date %in% timeline, input$timeline_state]
      date_subset_data <- data[data$Date %in% timeline, 1]
      
      # Combining data into a single dataset
      new_data <- as.data.frame(cbind(Date = as.Date(date_subset_data,
                                                     origin = "1970-01-01"), State = state_data))
      
      # Plotting
      ggplot(new_data, aes(x = as.Date(Date, origin = "1970-01-01"),
                           y = State)) + geom_line() +
        ggtitle(paste0("Daily confirmed cases in ", states)) +
        xlab("Date") + ylab("Confirmed cases")
      
    } else if(length(states) > 1){
      
      # Subsetting data based on dates selected by user
      state_data <- data[data$Date %in% timeline, input$timeline_state]
      date_subset_data <- data[data$Date %in% timeline, 1]
      
      # Combining data into a single dataset
      new_data <- as.data.frame(cbind(Date = date_subset_data, State = state_data))
      colnames(new_data) <- c("Date", states)
      
      # Converting data from long to tall to plot multiple lines in a single plot
      test_new_data <- melt(new_data, id = "Date")
      colnames(test_new_data) <- c("Date", "States", "value")
      # Plotting
      ggplot(test_new_data, aes(x = Date, y = value, colour = States)) +
        geom_line() +
        ggtitle(paste0("Daily confirmed cases in chosen states")) +
        xlab("Date") + ylab("Confirmed cases")
      
    }
  })
  
  # Plotting Wordmap
  output$wordmap <- renderPlot({
    
    set.seed(50)
    
    
    if(input$remove_covid == TRUE){
      
      twitter_dataset <- data[, c(53:ncol(data))]
      twitter_dataset <- twitter_dataset[, 
                    -which(colnames(twitter_dataset) %in% c("covid", 
                                                            "coronavirus",
                                                            "covid19"))]
      
    } else {
      
      # Dataset with only words
      twitter_dataset <- data[, c(53:ncol(data))]
      
    }

    
    # Defining timeframe to subset from data
    timeline <- seq(from = input$wordmap_date[1], to = input$wordmap_date[2],
                    by = "day")
    
    # Subsetting. 52 (date + 51 states -- we only want words)
    # word_data <- twitter_dataset[data$Date %in% timeline, 1:input$numb_words]
    word_data <- twitter_dataset[data$Date %in% timeline, 
                                 sample(colnames(twitter_dataset), input$numb_words,
                                        replace = FALSE)]
    
    # Now sum each column
    words <- colnames(word_data)
    word_sums <- colMeans(word_data)
    
    wordcloud(words = words, freq = word_sums, max.words = 411, random.order = FALSE,
              colors = brewer.pal(8, "Dark2"))
    
  })
  
}

# Loading the dashboard
shinyApp(ui, server)
