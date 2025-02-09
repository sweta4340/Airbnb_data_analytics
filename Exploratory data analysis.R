str(cleaned_listings_data)

#Analyzing prices with neighborhood location
plot <- ggplot(data=summary,aes(x = neighbourhood_cluster, y = mean_prices)) +
  geom_bar(stat="identity", position = "dodge") +  
  
  ggtitle("Price Vs Neighbourhood")  

plot + scale_x_continuous(
  limits = c(1,10),  # Set the limits of the x-axis
  breaks = seq(1, 10, 1),  # Set the tick marks on the x-axis
  labels = c("1", "2", "3", "4", "5", "6", "7","8","9","10"))
summary



summary <- cleaned_listings_data %>% group_by(neighbourhood_cleaned) %>% summarise(mean_prices=mean(price))


#Analyzing prices with neighborhood location

ggplot(data = summary , aes(x = neighbourhood_cleaned, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge")  +
  ggtitle("Price Vs Neighbourhood") 

#Analysing the prices per property type.
summary <- cleaned_listings_data %>% group_by(property_category) %>% summarise(mean_prices=mean(price))
ggplot(data = summary , aes(x = property_category, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge") + ggtitle("Price Vs Property_Category")


#Analysis of price vs number of bedrooms.
summary <- cleaned_listings_data %>% group_by(bedrooms) %>% summarise(mean_prices=mean(price))
summary <- summary[order(-summary$mean_prices), ]
summary
ggplot(data = summary , aes(x = mean_prices, y = reorder(bedrooms,mean_prices)))  + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Mean Prices")+
  ylab("Number of bedrooms")+ ggtitle("Price Vs Bedrooms")


#Analysis of price vs number of bathrooms.

levels(cleaned_listings_data$bathrooms_text)

summary <- cleaned_listings_data %>% group_by(bathrooms_text) %>% summarise(mean_prices=mean(price))
summary <- summary[order(-summary$mean_prices), ]
summary
ggplot(data = summary , aes(x = mean_prices, y = reorder(bathrooms_text,mean_prices)))  + 
  geom_bar(stat="identity", position = "dodge") +
  xlab("Mean Prices")+
  ylab("Bathrooms") + ggtitle("Price Vs Bathrooms")


#Analysis of price vs number of accommodates.
summary <- summary[order(-summary$mean_prices), ]
summary
summary <- cleaned_listings_data %>% group_by(accommodates) %>% summarise(mean_prices=mean(price))


ggplot(data = summary , aes(x = accommodates, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge") +  ggtitle("Price Vs Accommodates")




#Analysis of price vs review scores
summary <- cleaned_listings_data %>% group_by(number_of_reviews) %>% summarise(mean_prices=mean(price)) %>% filter(mean_prices<=500)
ggplot()+ geom_point(data = summary , aes(x = mean_prices, y = number_of_reviews))  + 
  geom_bar(stat="identity", position = "dodge") +  ggtitle("Price Vs Number of reviews") +coord



#Analysing the price distribution

summary(cleaned_listings_data$price)

cleaned_listings_data %>% ggplot(aes(price))+
  geom_density(fill = "deepskyblue", size = 1.5, color = "navyblue", alpha = 0.5)+
  xlab("Price")+
  ylab("Density")+
  ggtitle("Price Distribution of the listings")+
  theme(plot.title = element_text(hjust = 0.5))

sum(cleaned_listings_data$price <=500)/length(cleaned_listings_data$price)

cleaned_listings_data %>% filter(price <=500) %>% ggplot(aes(price))+
  geom_density(fill = "deepskyblue", size = 1.5, color = "navyblue", alpha = 0.5)+
  xlab("Price")+
  ylab("Density")+
  ggtitle("Price Distribution of the listings")+
  theme(plot.title = element_text(hjust = 0.5))






property_type_count <- table(cleaned_listings_data$property_category)


# Create a data frame for ggplot
df <- data.frame( Property_type = names(property_type_count), Count = property_type_count)

# Plot the pie chart
ggplot(df, aes(x = "", y = Count.Freq, fill = Property_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Distribution of Airbnb Listings by Room Type")

summary <- cleaned_listings_data %>% group_by(has_availability==t) %>% summarise(Count=n())
ggplot(data = summary , aes(x = neighbourhood_cluster, y = Count))  + 
  geom_bar(stat="identity", position = "dodge") +  ggtitle("Number of listings per neighborhood cluster")


# Assuming 'availability_365' and 'neighborhood_cleaned' are the columns in your dataset
ggplot(cleaned_listings_data, aes(x = neighbourhood_cleaned, y = availability_365)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Availability vs. Neighborhood",
       x = "Neighborhood",
       y = "Availability (in days)")












