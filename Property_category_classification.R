library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
#read calendar data
listings_data <- read.csv("C:/Users/KRISHNA/Desktop/listings.csv")

unique(listings_data$property_type)

summary_property_type <- listings_data %>% group_by(property_type) %>% summarise(count=n()) %>% arrange() %>% view()
data <- select(listings_data,property_type)

# Categorize data based on the presence of keys
data <- data %>%
  mutate(property_category = case_when(
    str_detect(property_type, "Entire") ~ "Entire property",
    str_detect(property_type, "Private") ~ "Private Room",
    str_detect(property_type, "Shared") ~ "Shared Room",
    TRUE ~ "Other"))
data %>% view()
summary_category <- data %>% group_by(property_category) %>% summarise(count=n())
ggplot(data = summary_category , aes(x = property_category, y = count))  + 
  geom_bar(stat="identity", position = "dodge")

unique(listings_data$license) %>% view