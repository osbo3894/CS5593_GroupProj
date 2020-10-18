# Libraries used
library(shiny)
library(shinydashboard)
library(ggplot2)

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

    timeline <- seq(from = input$timeline_date[1], to = input$timeline_date[2],
              by = "day")
    
    state_data <- data[timeline %in% data$Date, input$timeline_state]
    date_subset_data <- data[data$Date %in% timeline, 1]
    
    print(date_subset_data)
    
    new_data <- as.data.frame(cbind(Date = date_subset_data, State = state_data))
    
    print(new_data)
    
    ggplot(new_data, aes(x = as.Date(Date, origin = "1970-01-01"), 
                         y = State)) + geom_line() +
      ggtitle(paste0("Daily confirmed cases in ", input$timeline_state)) +
      xlab("Date") + ylab("Confirmed cases")

  })
  
}

# Loading the dashboard
shinyApp(ui, server)
