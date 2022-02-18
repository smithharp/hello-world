
setwd('C:/Users/HS/Desktop/School/CSC324 Software dev/Data Visualization')
library(tidyverse)

disasters <- read.csv('disasters.csv')

#head(disasters)


complete1 <- filter(disasters, is.na(Total.Deaths) == FALSE, is.na(Total.Deaths) == FALSE)


#ggplot(avocado, aes(region, colour = type)) + geom_bar()
ggplot(disasters, aes(x = Year, y = Total.Deaths, colour = Disaster.Subgroup)) + geom_point()

ggplot(filter(disasters, Disaster.Subgroup == "Geophysical"), aes(x = Disaster.Subtype, y = Total.Deaths, colour = Disaster.Type)) + geom_jitter() + scale_y_continuous(trans = 'log10')

ggplot(disasters, aes(x = Start.Month, fill = Disaster.Type)) + geom_bar(position = position_dodge())

ggplot(disasters, aes(x = Year, y = count)) + geom_line()

#plotting disasters over time
ggplot(disasters, aes(x = Year, colour = Disaster.Subgroup)) + geom_line(stat = "bin", size = 1, binwidth = 2)
ggplot(disasters, aes(x = Year, colour = Disaster.Type)) + geom_line(stat = "bin", size = 1, binwidth = 10)

#disasters by continent
ggplot(disasters, aes(x = Continent, fill = Disaster.Subgroup)) + geom_bar(position = position_dodge())
ggplot(filter(disasters, Continent == 'Africa'), aes(x = Disaster.Type, fill = Disaster.Subgroup)) + geom_bar()
ggplot(filter(disasters, Total.Deaths < 10000), aes(x = Continent, y = Total.Deaths, colour = Disaster.Subgroup)) + geom_jitter()

#disasters by country
ggplot(disasters, aes(x = Country)) + geom_bar() + xlim(0,10)






