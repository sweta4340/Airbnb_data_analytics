library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
#read calendar data
calendar_data <- read.csv("C:/Users/KRISHNA/Desktop/calendar.csv")

# convert char date into Date format
#calendar_data$date <- as.Date(calendar_data$date)

#checking null values in all columns
colSums(is.na(calendar_data))

#checking datatypes of all columns
str(calendar_data)

#converting char price into numeric value by removing chrs
calendar_data$price <- as.numeric(gsub('[$,]', '', calendar_data$price))
unique(calendar_data$price)
head(calendar_data)

#adding new column called month
calendar_data$month<- format(as.Date(calendar_data$date, format="%Y-%m-%d"),"%m")
head(calendar_data)
str(calendar_data)
calendar_data$month <- as.numeric(calendar_data$month)
unique(calendar_data$month)
#adding new called season based on the month number
calendar_data <- calendar_data %>% mutate(Season=if_else(month==12 | month==1 | month==2,"Winter",
                                                       if_else(month>2 & month<=5,"Spring",
                                                      if_else(month>5 & month<=8,"Summer","Fall"))))
head(calendar_data)
unique(calendar_data$Season)

#Analyzing average prices by season and availability
summary <- calendar_data %>% group_by(Season,available) %>% summarise(average_price=mean(price))
ggplot(data=summary, aes(x=Season, y=average_price, fill=available)) + geom_bar(stat="identity", position = "dodge") +
  ggtitle("Analysing price by season and availability")  +  theme(
    plot.margin = margin(1, 1, 1, 1, "cm")
  )
#Analyzing average prices by months and availability
summary <- calendar_data %>% group_by(month,available) %>% summarise(average_price=mean(price),
                                                                     median_price=median(price))
ggplot(data=summary, aes(x=month, y=average_price, fill=available)) + geom_bar(stat="identity", position = "dodge") +
  ggtitle("Analysing price by months")


#Analyzing median and average of prices over months
summary <- calendar_data %>% group_by(month) %>% summarise(average_price=mean(price),
                                                           median_price=median(price))
ggplot(summary, aes(x=month)) + 
  geom_line(aes(y = average_price), color = "darkred") + 
  geom_line(aes(y = median_price), color="steelblue", linetype="twodash") 


#Analyzing average price of a listing by months
summary <- calendar_data %>% filter(listing_id==2269269) %>% group_by(month,available) %>% summarise(average_price=mean(price))
ggplot(data=summary, aes(x=month, y=average_price, fill=available)) + geom_bar(stat="identity", position = "dodge") +
  ggtitle("Analysing price of a listing by months")


#Occupancy by months
summary <- calendar_data %>% group_by(Season) %>% summarise(Occupancy = sum(!(calendar_data$available =="t")))

ggplot(summary,aes(x=Season,y=Occupancy)) + geom_bar(stat="identity", position = "dodge") +
  ggtitle("Analysing occupancy by Seasons")




library(randomForest)
library(ggplot2)

# Load or prepare your dataset (replace this with your dataset)
# For this example, we'll use the built-in 'iris' dataset
data(iris)

# Split the dataset into features and target
features <- iris[, 1:4]  # Select the first 4 columns as features
target <- iris[, 5]     # The 5th column (Species) is the target

# Fit a Random Forest model
rf_model <- randomForest(x = features, y = target, ntree = 100, importance = TRUE)

# Get feature importance scores
feature_importance <- importance(rf_model)

# Sort the feature importance scores in descending order
#sorted_importance <- feature_importance[order(-feature_importance), ]

# Create a bar chart to visualize feature importance
ggplot(data = feature_importance, aes(x = rownames(feature_importance), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Features", y = "Mean Decrease in Gini Index") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




