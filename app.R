#
# Rafiya Awan
# CS 424 - Spring 2022
# Shiny web application for CTA Ridership Data
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(dplyr)
library(DT)

#read in UIC data
uicData <- read.csv(file = "UIC_Data.csv", header = TRUE)

#Re-format date field
uicData$newDate <- mdy(uicData$date)
uicData$date <- NULL

# Add up entries at UIC-Halsted for each year 
byYearUIC <- setNames(aggregate(uicData$rides, by=list(format(uicData$newDate, "%Y")), sum), c("Year", "Entries"))

#read in O'Hare data
ohareData <- read.csv(file = "Ohare_Data.csv", header = TRUE)

#Re-format date field
ohareData$newDate <- mdy(ohareData$date)
ohareData$date <- NULL

# Add up entries at UIC-Halsted for each year 
byYearOhare <- setNames(aggregate(ohareData$rides, by=list(format(ohareData$newDate, "%Y")), sum), c("Year", "Entries"))


#For year input
years <- c(2001:2021)

# Define UI for application dashboard
ui <- dashboardPage(
  
  dashboardHeader(title = "CTA Ridership"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   
                   sidebarMenu(
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("", tabName = "cheapBlankSpace", icon = NULL),
                     menuItem("About", tabName = "About", icon = NULL),
                     menuItem("Data Visualizations", tabName = "Datavisualizations", icon = NULL)),
                   
                   selectInput("Year", "Select the year to visualize for UIC-Halsted", years, selected = 2021),
                   selectInput("Display", "View Charts or Tables", c("Charts","Tables"), selected = "Charts")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
              h1("CTA Ridership Project"),
              h2("Data From: Chicago Data Portal at https://data.cityofchicago.org/Transportation/CTA-Ridership-L-Station-Entries-Daily-Totals/5neh-572f"),
              h2("Application Written by Rafiya Awan for CS 424")
      ),
      tabItem(tabName = "Datavisualizations",
    conditionalPanel(
      condition = "input.Display == 'Charts'",
    fluidRow(
      column(6,
             fluidRow(
               box(title = "Entries at UIC-Halsted for Each Year", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("uicYearPlot", height = 600)
               )
             )
      ),
      column(6,
             fluidRow(
               box(title = "Entries at UIC-Halsted Each Day over Year", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("uicDayPlot", height = 600)
               )
             )
      )
    ),
    fluidRow(
      column(6,
             fluidRow(
               box(title = "Entries at UIC-Halsted Each Month over Year", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("uicMonthPlot", height = 600)
               )
             )
      ),
      column(6,
             fluidRow(
               box(title = "Entries at UIC-Halsted Each Day of the Week over Year", solidHeader = TRUE, status = "primary", width = 12,
                   plotOutput("uicDayofWeekPlot", height = 600)
               )
             )
      )
    )),
    conditionalPanel(
      condition = "input.Display == 'Tables'",
    fluidRow(
      column(6,
             fluidRow(
               box(title = "UIC-Halsted Entries for Each Year as Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("uicYearTab", height = 600)
               )
             )
      ),
      column(6,
             fluidRow(
               box(title = "Entries at UIC-Halsted Each Day over Year as Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("uicDayTab", height = 600)
               )
             )
      )
      
    ),
    fluidRow(
      column(6,
             fluidRow(
               box(title = "UIC-Halsted Entries for Each Month over Year as Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("uicMonthTab", height = 600)
               )
             )
      ),
      column(6,
             fluidRow(
               box(title = "UIC-Halsted Entries for Each Day of the Week over Year as Table", solidHeader = TRUE, status = "primary", width = 12,
                   dataTableOutput("uicDayofWeekTab", height = 600)
               )
             )
      )
      
    )))
  )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    # Data for one year
    entriesInOneYearReactive <- reactive({subset(uicData, year(uicData$newDate) == input$Year)})
    
    # Entries at UIC-Halsted for Each Year
    output$uicYearPlot <- renderPlot({
      
      ggplot(byYearUIC, aes(x=Year, y=Entries)) + 
        geom_bar(stat="identity", width=0.7, fill="steelblue") + 
        labs(x="Year", y="Total Entries")
    })
    
    # Entries at UIC-Halsted Each Day of Given Year
    output$uicDayPlot <- renderPlot({
      
      entriesInOneYear <- entriesInOneYearReactive()
      
      ggplot(entriesInOneYear, aes(x=newDate, y=rides)) + 
        geom_bar(stat="identity", width=0.7, fill="steelblue") + 
        labs(x=paste("Day in", input$Year), y="Total Entries")
    })
    
    #Entries at UIC-Halsted Each Month of Given Year
    output$uicMonthPlot <- renderPlot({
  
      entriesInOneYear <- entriesInOneYearReactive()
      
      # Add up entries at UIC-Halsted for each Month of Year
      byMonthUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(format(entriesInOneYear$newDate, "%b")), sum), c("Month", "Entries"))
      byMonthUIC$Month <- factor(x=byMonthUIC$Month, levels=month.abb, ordered=TRUE)    
      
      ggplot(byMonthUIC, aes(x=Month, y=Entries)) +
        geom_bar(stat="identity", width=0.7, fill="steelblue") +
        labs(x=paste("Month in", input$Year), y="Total Entries")
    })
    
    #Entries at UIC-Halsted Each Day of the Week of Year
    output$uicDayofWeekPlot <- renderPlot({
      
      entriesInOneYear <- entriesInOneYearReactive()
      entriesInOneYear$weekday <- wday(entriesInOneYear$newDate, label=TRUE)
      
      # Add up entries at UIC-Halsted for each Day of the Week of Year
      byDayofWeekUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(entriesInOneYear$weekday), sum), c("Day", "Entries"))
      
      ggplot(byDayofWeekUIC, aes(x=Day, y=Entries)) + 
        geom_bar(stat="identity", width=0.7, fill="steelblue") + 
        labs(x=paste("Day of the Week in", input$Year), y="Total Entries")
    })
    
    # Table - Entries at UIC-Halsted for Each Year
    output$uicYearTab <- DT::renderDataTable(
      DT::datatable(byYearUIC, 
      options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
      ), rownames = FALSE 
      )
    )
    
    # Table - Entries at UIC-Halsted Each Day of Given Year
    output$uicDayTab <- DT::renderDataTable(
      DT::datatable({
        entriesInOneYear <- entriesInOneYearReactive()
        
        names(entriesInOneYear)[5] <- 'Date'
        names(entriesInOneYear)[4] <- 'Entries'
        select(entriesInOneYear, Date, Entries)
      }, 
      options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
      ), rownames = FALSE 
      )
    )
    
    # Table - Entries at UIC-Halsted Each Month of Given Year
    output$uicMonthTab <- DT::renderDataTable(
      DT::datatable({
        entriesInOneYear <- entriesInOneYearReactive()
        
        # Add up entries at UIC-Halsted for each Month of 2021
        byMonthUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(format(entriesInOneYear$newDate, "%b")), sum), c("Month", "Entries"))
        byMonthUIC$Month <- factor(x=byMonthUIC$Month, levels=month.abb, ordered=TRUE)    
        
        entriesMonth <- as.data.frame(byMonthUIC)
      }, 
      options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
      ), rownames = FALSE 
      )
    )
    
    #Table - Entries at UIC-Halsted Each Day of the Week of Year
    output$uicDayofWeekTab <- DT::renderDataTable(
      DT::datatable({
        entriesInOneYear <- entriesInOneYearReactive()
        entriesInOneYear$weekday <- wday(entriesInOneYear$newDate, label=TRUE)
        
        # Add up entries at UIC-Halsted for each Day of the Week of Year
        byDayofWeekUIC <- setNames(aggregate(entriesInOneYear$rides, by=list(entriesInOneYear$weekday), sum), c("Day", "Entries"))
        
        entriesDayofWeek <- as.data.frame(byDayofWeekUIC)
      }, 
      options = list(searching = FALSE, pageLength = 8, lengthChange = FALSE, order = list(list(0, 'asc'))
      ), rownames = FALSE 
      )
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
