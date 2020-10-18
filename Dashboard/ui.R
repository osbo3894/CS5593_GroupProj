# Libraries used
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Exploratory Data Analysis", tabName = "EDA", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "EDA")
  ),
  # Row 1
  fluidRow(
    box(plotOutput("covid_timeline", height = 250)),
    # box(tableOutput("data")),
    box(title = "Timeline inputs",
        selectInput("timeline_state", "Choose a state:",
                    c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                      "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                      "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                      "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                      "West Virginia", "Wisconsin", "Wyoming")),
        
        dateRangeInput("timeline_date", "Choose date range:",
                       start = "2020-04-13",
                       end = "2020-09-19",
                       min = "2020-04-13",
                       max = "2020-09-19",
                       format = "yy/mm/dd",
                       separator = " - "))
  )
)


dashboardPage(skin = "black",
  dashboardHeader(title = "COVID-19 Dashboard"),
  sidebar,
  body
)