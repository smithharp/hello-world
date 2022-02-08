shrooms <- 'mushrooms.csv'


ggplot(shrooms, aes(cap.color, colour = class)) + geom_bar(position = position_dodge())