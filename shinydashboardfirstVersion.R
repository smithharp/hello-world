

setwd('C:/Users/HS/Desktop/School/CSC324 Software dev/Data Visualization')
library(tidyverse)

disasters <- read.csv('disasters.csv')
#scatterplot, deaths by time
plot1 <- ggplot(disasters, aes(x = Year, y = Total.Deaths, colour = Disaster.Subgroup)) + geom_point() + scale_y_continuous(trans = 'log10')



#shinydashboard
setwd('C:/Users/HS/Desktop/School/CSC324 Software dev/Data Visualization')
library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Hello world"),
  dashboardSidebar(
    sidebarMenu(
      #id = "tabs",
      menuItem("TEST!", tabName = "test1"),
      menuItem("Summary", tabName = "summary"),
      menuItem("Europe", tabName = "europe", icon = NULL))
    
  ),
  dashboardBody(
    tabItems(
      #id = "tabs",
      tabItem(tabName = "test1",
              fluidRow(
                div(title = "test1")
              )),
    ),
      tabItem(tabName = "summary", fluidRow(
        h1 = "Summary",
        div(title = "Disaster events over time", plotOutput("plot1")),
        div(title =  "Disaster events by subgroup", plotOutput("plot2")),
        div(title = "Disaster events by continent", plotOutput("plot3")),
        div(title = "Disaster events by time of year", plotOutput("plot4"))
      ))
    
  )
)

server <- function(input, output) {
  
  output$plot1 <- renderPlot(
    
    ggplot(disasters, aes(x = Year, y = Total.Deaths, colour = Disaster.Subgroup)) + geom_point() + scale_y_continuous(trans = 'log10')
  )
  output$plot2 <- renderPlot(ggplot(disasters, aes(x = Disaster.Subgroup, y = Total.Deaths, colour = Disaster.Type)) + geom_jitter() + scale_y_continuous(trans = 'log10')
  )
  output$plot3 <- renderPlot(ggplot(disasters, aes(x = Continent, fill = Disaster.Subgroup)) + geom_bar(position = position_dodge())
  )
  output$plot4 <- renderPlot(ggplot(disasters, aes(x = Start.Month, fill = Disaster.Type)) + geom_bar(position = position_dodge())
  )
}

shinyApp(ui, server)

