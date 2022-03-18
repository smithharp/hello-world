


#shinydasboard
setwd('C:/Users/HS/Desktop/School/CSC324 Software dev/Data Visualization')
library(tidyverse)
library(shinydashboard)
library(shiny)
library(plotly)
library(RColorBrewer)

disasters <- read.csv('disasters.csv')



ui <- dashboardPage (
  dashboardHeader(title = "Global Disasters 1900-2021"),
  dashboardSidebar(
    sidebarMenu(
      #menuItems(
        menuItem(tabName = "overview",
                 text = "Overview"),
        menuItem(tabName = "europe",
                 text = "Europe"),
        menuItem(tabName = "summaries",
                 text = "Summaries"),
        menuItem(tabName = "options",
                 text = "Options")
    )
    
  ),
  dashboardBody(
    tabItems(
        tabItem(tabName = "overview",
                fluidRow(
                  box(plotOutput("ov1")),
                  box(plotOutput("ov2")),
                  box(plotOutput("ov3")),
                  #this is the animation
                  box(plotlyOutput("ov5"))
                )
                ),
        tabItem(tabName = "europe",
                fluidPage(
                  fluidRow(
                    box(plotOutput("eu1")),
                    box(plotOutput("eu2")),
                    box(plotOutput("eu3")),
                    box(plotOutput("eu4")),
                    box(plotOutput("eu5"))
                  )
                )),
        tabItem(tabName = "summaries",
                fluidPage(
                  fluidRow(
                    box(plotOutput("sum1")),
                    box(plotOutput("sum2")),
                    box(plotOutput("sum3")),
                    box(plotOutput("sum4"))
                  )
                ))
        )
  )
  )

server <- function(input, output) {
  
  
  #OVERVIEW PLOTS
  #Scatterplot total deaths
  output$ov1 <- renderPlot(deathscatterplot)
  #Jitterplot of total deaths
  output$ov2 <- renderPlot(deathjitterplot)
  #Bar plot of disaster subgroups
  output$ov3 <- renderPlot(disastersubgroups)
  output$ov5 <- renderPlotly(subgroupsbyseason)
  
  #EUROPE PLOTS
  
  output$eu1 <- renderPlot(euroscatter)
  output$eu2 <- renderPlot(eurosubgroup)
  output$eu3 <- renderPlot(eurocommon)
  output$eu4 <- renderPlot(euroseasonal)
  output$eu5 <- renderPlot(eurodeathss)
  
  #SUMMARY PLOTS
  output$sum1 <- renderPlot(mostdisasters)
  output$sum2 <- renderPlot(mostdeaths)
  output$sum3 <- renderPlot(combocombo)
  output$sum4 <- renderPlot(leastdeaths)
  
  #broken one
  output$broken <- renderPlotly(broken)
  
  
}

shinyApp(ui, server)

#OVERVIEW PLOTS
#1
deathscatterplot <- ggplot(disasters, aes(x = Year, y = Total.Deaths, colour = Disaster.Subgroup)) + geom_point() + scale_y_continuous(trans = 'log10')
#2
deathjitterplot <- ggplot(disasters, aes(x = Disaster.Subgroup, y = Total.Deaths, colour = Disaster.Type)) + geom_jitter() + scale_y_continuous(trans = 'log10')
#3
disastersubgroups <- ggplot(disasters, aes(x = Continent, fill = Disaster.Subgroup)) + geom_bar(position = position_dodge())


#building animation here
groupd_season <- group_by(disasters, Continent, Start.Month, Disaster.Subgroup)
animation_season <- filter(groupd_season, is.na(Start.Month) == FALSE, Disaster.Subgroup != "Extra-terrestrial")
animation_season <- count(animation_season)

subgroupsbyseason <- animation_season%>%
  plot_ly(
    x = ~Continent,
    y = ~n,
    color = ~Disaster.Subgroup,
    frame = ~Start.Month,
    type = 'bar'
  )


#EUROPE PLOTS
#filter thing for europe
europe <- filter(disasters, Continent == "Europe")

#scatterplot, all disasters in Europe
euroscatter <- ggplot(europe, aes(x = Year, y = Total.Deaths, color = Disaster.Subgroup)) + geom_point() + scale_y_continuous(trans = "log10")

#bar plot, disasters by subgroup
eurosubgroup <- ggplot(europe, aes(x = Disaster.Subgroup)) + geom_bar(fill = "darkblue")

#filter out the less common ones so we can focus on what is important
bigtypes <- filter(europe,
                   Disaster.Type != "Drought",
                   Disaster.Type != "Epidemic",
                   Disaster.Type != "Fog",
                   Disaster.Type != "Impact",
                   Disaster.Type != "Insect infestation",
                   Disaster.Type != "Mass movement (dry)",
                   Disaster.Type != "Volcanic activity",
                   is.na(Start.Month) == FALSE)
#plot the more common disaster types
eurocommon <- ggplot(bigtypes, aes(x = Disaster.Type, fill = Disaster.Type)) + geom_bar() + scale_fill_brewer(palette = "PuBu")

#same as above but by death
eurodeathss <- ggplot(bigtypes, aes(x = Disaster.Type, fill = Disaster.Type, y = Total.Deaths)) + geom_col() + scale_fill_brewer(palette = "PuBu")


#line plot, disaster types by season
euroseasonal <- ggplot(bigtypes, aes(x = Start.Month, color = Disaster.Type)) + geom_line(stat = "bin", binwidth = 1, size = 1) + scale_color_brewer(palette = "Set2")



#Europe animation
groupd_europe <- group_by(bigtypes, Continent, Start.Month, Disaster.Type)
euro_season <- filter(groupd_europe, is.na(Start.Month) == FALSE, Disaster.Subgroup != "Extra-terrestrial", Continent == "Europe")
euro_season <- count(euro_season)

euroseason <- euro_season%>%
  plot_ly(
    x = ~Disaster.Type,
    y = ~n,
    color = ~Disaster.Type,
    frame = ~Start.Month,
    type = 'bar'
  )






#summaries
tops <- group_by(disasters, Continent, Country)
tops <- count(tops)
tops <- arrange(tops, desc(n))
mostdisasters <- ggplot(head(tops, 10), aes(x = Country, y = n, fill = Continent)) + geom_col()
lesd <- ggplot(head(bottoms, 10), aes(x = Country, y = n, fill = Continent)) + geom_col()


topdeath <- filter(disasters, is.na(Total.Deaths) == FALSE)
topdeath <- group_by(topdeath, Continent, Country)
topdeath <- summarize(topdeath, d = sum(Total.Deaths))
bottomdeath <- arrange(topdeath, d)
topdeath <- arrange(topdeath, desc(d))

mostdeaths <- ggplot(head(topdeath, 10), aes(x = Country, y = d, fill = Continent)) + geom_col() + scale_color_brewer(palette = "PuRd")
leastdeaths <- ggplot(head(bottomdeath, 10), aes(x = Country, y = d, fill = Continent)) + geom_col() + scale_color_brewer(palette = "PuRd")



scatterdeaths <- filter(disasters, is.na(Total.Deaths) == FALSE)
scatterdeaths <- group_by(scatterdeaths, Country)
scatterdeaths <- count(scatterdeaths)

combo1 <- arrange(topdeath, Country)
combo2 <- arrange(scatterdeaths, Country)
combodeath <- cbind(combo1, combo2)
combodeath <- select(combodeath, 1, 2, 3, 5)

combocombo <- ggplot(combodeath, aes(x = n, y = d, color = Continent)) + geom_point() + scale_y_continuous(trans = "log10")









