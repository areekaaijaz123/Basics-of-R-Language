library(dplyr)
library(ggplot2)

#Data extraction generating random sample of 5000
used_cars = read.csv("E:/1. Data Science NextGen/Data sets/Autos_Data.csv",stringsAsFactors = FALSE , na.strings=c("","NA"))
set.seed(10)
used_cars <- sample_n(used_cars,5000)

#Removing fruitless columns
used_cars <- used_cars %>% select(-c(dateCrawled,seller,offerType,abtest,dateCreated,nrOfPictures,postalCode,lastSeen))

#Removing duplicate rows
used_cars <- used_cars %>% distinct()

##write.csv(used_cars,'E:/1. Data Science NextGen/Data Sets/Autos_Data.csv',row.names = FALSE)

#Visualization

#Bar Graph

used_cars %>% group_by(gearbox) %>% summarise(count = n())
used_cars %>% ggplot(aes(gearbox)) + geom_bar(fill = "steelblue")

used_cars %>% group_by(brand) %>% summarise(count = n())
used_cars %>% ggplot() + geom_bar(aes(brand) ,fill = "steelblue") + coord_flip()

#Scatter Plot

used_cars %>% ggplot() + geom_point(aes(kilometer,price/10^5, color = gearbox)) + scale_y_continuous(limits = c(0,1))

used_cars %>% ggplot(aes(kilometer,powerPS, color = vehicleType)) + geom_point() + scale_y_continuous(limits = c(0,750))

used_cars %>% ggplot(aes(kilometer,powerPS, color = vehicleType , size = price)) + geom_point() + scale_y_continuous(limits = c(0,750))

used_cars %>% ggplot() + geom_point(aes(gearbox,kilometer, color = vehicleType)) + facet_wrap(~yearOfRegistration)

used_cars %>% ggplot(aes(kilometer, powerPS)) + geom_point() + geom_smooth(method = 'lm') + scale_y_continuous(limits = c(0,750))

used_cars %>% ggplot(aes(kilometer, powerPS)) + geom_point() + scale_y_continuous(limits = c(0,750)) + geom_line()

#Box Plot

used_cars %>% ggplot(aes(vehicleType,powerPS)) + geom_boxplot(fill = "steelblue") + scale_y_continuous(limits = c(0,750))


used_cars %>% ggplot() + geom_boxplot(aes(gearbox,powerPS), fill = "lightblue") + scale_y_continuous(limits = c(0,750))

#Violin Plot

used_cars %>% ggplot(aes(vehicleType,powerPS)) + geom_violin(fill = "steelblue") + scale_y_continuous(limits = c(0,750))

