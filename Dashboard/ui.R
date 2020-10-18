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
    box(title = "Confirmed cases over time", solidHeader = TRUE,
        collapsible = TRUE, status = "primary",
        
        plotOutput("covid_timeline", height = 300)),
    
    box(title = "Inputs", solidHeader = TRUE,
        collapsible = TRUE, status = "primary", height = 362,
        
        helpText("This plot represents the daily confirmed",
        "COVID-19 cases in each state plus the District of Columbia.",
        "You may plot up to 5 states at the same time. To delete",
        "a selected state, click on it and press 'backspace' on your",
        "keyboard.",
        "In addition, you can also modify the timeline between two",
        "particular dates from 04/13/2020 to 09/19/2020 to investigate",
        "changes over time"),
        
        selectizeInput("timeline_state", "Choose up to five states:",
                    c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
                      "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                      "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                      "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                      "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                      "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                      "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
                      "West Virginia", "Wisconsin", "Wyoming"),
                    options = list(maxItems = 5),
                    selected = "Alabama"),
        
        dateRangeInput("timeline_date", "Choose date range:",
                       start = "2020-04-13",
                       end = "2020-09-19",
                       min = "2020-04-13",
                       max = "2020-09-19",
                       format = "yy/mm/dd",
                       separator = " - "))
  ),
  
  fluidRow(
    box(title = "Twitter words frequency over time", solidHeader = TRUE,
        collapsible = TRUE, status = "success",
               
        plotOutput("wordmap", height = 500)),
  
  box(title = "Inputs", solidHeader = TRUE,
      collapsible = TRUE, status = "success", height = 560,
      
      helpText("This plot represents the most commonly used words in",
               "COVID-19 related tweets averaged over time.",
               "You may plot up to 411 words at the same time. They are drawn",
               "in no particular order from the dataset.",
               "In addition, you can also modify the timeline between two",
               "particular dates from 04/13/2020 to 09/19/2020 to investigate",
               "changes over time.",
               "Lastly, you can remove COVID-19 references from the wordmap",
               "(e.g. covid, covid19, coronavirus) from the wordmap."),
      
      numericInput("numb_words", "Number of words to populate map",
                   min = 10, max = 411, value = 10),
      
      dateRangeInput("wordmap_date", "Choose date range:",
                     start = "2020-04-13",
                     end = "2020-09-19",
                     min = "2020-04-13",
                     max = "2020-09-19",
                     format = "yy/mm/dd",
                     separator = " - "),
      
      checkboxInput("remove_covid", "Remove COVID-19 references", value = FALSE))
    )
  )


dashboardPage(skin = "black",
  dashboardHeader(title = "COVID-19 Dashboard"),
  sidebar,
  body
)