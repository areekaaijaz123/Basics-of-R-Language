library(dplyr)
library(ggplot2)
library(tidyverse)

#Data Extraction
autos_data = read.csv("E:/1. Data Science NextGen/Data sets/autos.csv",stringsAsFactors = FALSE, na.strings=c("","NA"))
view(autos_data)

#Data summary
summary(autos_data)

#Data inspection
str(autos_data)

#Identifying uniques
unique(autos_data$seller)
unique(autos_data$abtest)
unique(autos_data$offerType)

#Data Cleaning and Data wrangling

#Dropping some fruitless columns
autos_data %>% select(-c(dateCrawled,seller,offerType,abtest,dateCreated,nrOfPictures,postalCode,lastSeen)) -> autos_data
view(autos_data)

#Filtering data 
autos_data %>% filter(yearOfRegistration >= 2017) %>% nrow()->new_cars
autos_data %>% filter(yearOfRegistration <= 1950) %>% nrow()->old_cars
autos_data %>% filter(price <= 100) %>% nrow()->cheap_cars
autos_data %>% filter(price >= 150000) %>% nrow()->expensive_cars
autos_data %>% filter(powerPS < 10) %>% nrow()->lowPower_cars
autos_data %>% filter(powerPS > 500) %>% nrow()->highPower_cars

cat("No. of new cars : ",new_cars,"\n")
cat("No. of old cars : ",old_cars,"\n")
cat("No. of cheap cars : ",cheap_cars,"\n")
cat("No. of expensive cars : ",expensive_cars,"\n")
cat("No. of cars with low power : ",lowPower_cars,"\n")
cat("No. of cars with high power : ",highPower_cars,"\n")

#Removing duplicate rows
autos_data %>% distinct() -> autos_data

#Observing null values
#autos_data %>% select(everything()) %>% summarise_all(funs(sum(is.na(.))))
colSums(is.na(autos_data))
autos_data[is.na(autos_data)] <- "not declared"

#Data Visualization

#Visualization using bar graph
autos_data %>% group_by(gearbox) %>% summarise(count = n())
autos_data %>% ggplot(aes(gearbox)) + geom_bar(fill = "steelblue") + scale_y_continuous(limits = c(0,150000))

autos_data %>% group_by(brand) %>% summarise(count = n())
autos_data %>% ggplot(aes(brand)) + geom_bar(fill = "steelblue") + scale_y_continuous(limits = c(0,40000)) + coord_flip()

autos_data %>% group_by(vehicleType) %>% summarise(count = n())
autos_data %>% ggplot(aes(vehicleType)) + geom_bar(fill = "steelblue") + scale_y_continuous(limits = c(0,50000))

autos_data %>% group_by(fuelType) %>% summarise(count = n())
autos_data %>% ggplot(aes(fuelType)) + geom_bar(fill = "steelblue") + scale_y_continuous(limits = c(0,120000))

autos_data %>% group_by(notRepairedDamage) %>% summarise(count = n())
autos_data %>% ggplot(aes(notRepairedDamage)) + geom_bar(fill = "steelblue") + scale_y_continuous(limits = c(0,150000))

#Splitting dataset into training and testing datasets 
sample.split(autos_data$price,SplitRatio = 0.80) -> split_model
summary(split_model)

subset(autos_data,split_model==T) -> train
nrow(train)

subset(autos_data,split_model==F) -> test
nrow(test)

model = lm(price ~ vehicleType + yearOfRegistration + gearbox + powerPS + model + kilometer + fuelType + brand + notRepairedDamage, data = train)
summary(model)

result = cbind(actualValue = test$price , predictedValue = predict(model,test))
view(result)

