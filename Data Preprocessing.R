library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(randomForest)

#Reading the cleaned data after performing data cleaning.
cleaned_listings_data <- read.csv("C:/Users/KRISHNA/Desktop/SHWETA/Fall 2023/BA/Project/Price prediction/Dataset/Cleaned_Listings_data.csv")

#Analysing the data.
head(cleaned_listings_data) %>% View()


#Categorizing the variable host_response rate into >75%, 50%-75%, 25%-50%a and <25% for better understanding.
cleaned_listings_data <- cleaned_listings_data %>% mutate("host_response_rate"= ifelse(cleaned_listings_data$host_response_rate=='N/A',"0%",cleaned_listings_data$host_response_rate))
  #Converting the variable into numeric value.
cleaned_listings_data$host_response_rate <- as.numeric(sub("%", "",cleaned_listings_data$host_response_rate))
cleaned_listings_data <- cleaned_listings_data %>%
  mutate("host_response_rate"= if_else(host_response_rate>=75,">75%",
                                                       if_else(host_response_rate<75 & host_response_rate>50,"50%-75%",
                                                               if_else(host_response_rate<=50 & as.numeric(host_response_rate)>25,"25%-50%","<25%"))))


#Categorizing the variable host_response rate into >75%, 50%-75%, 25%-50%a and <25% for better understanding.
cleaned_listings_data <- cleaned_listings_data %>% mutate("host_acceptance_rate"= ifelse(cleaned_listings_data$host_acceptance_rate=='N/A',"0%",cleaned_listings_data$host_acceptance_rate))
  #Converting the variable into numeric value.
cleaned_listings_data$host_acceptance_rate <- as.numeric(sub("%", "",cleaned_listings_data$host_acceptance_rate))
cleaned_listings_data <- cleaned_listings_data %>%
  mutate("host_acceptance_rate"= if_else(host_acceptance_rate>=75,">75%",
                                       if_else(host_acceptance_rate<75 & host_acceptance_rate>50,"50%-75%",
                                               if_else(host_acceptance_rate<=50 & as.numeric(host_acceptance_rate)>25,"25%-50%","<25%"))))



#Analysing the geography of listings using the variables latitude and longitude.

  neighbourhood_data <- select(cleaned_listings_data, latitude, longitude)

  #To determine optimal value of K
  scaled_data <- scale(neighbourhood_data)

  # Function to calculate total within-cluster sum of squares
  within_cluster_sum_of_squares <- function(k) {
    kmeans_result <- kmeans(scaled_data, centers = k)
    kmeans_result
    return(sum(kmeans_result$withinss))
  }

  # Try different values of k and calculate the total within-cluster sum of squares
  k_values <- 1:10  # Trying different values of k
  withiness_for_k <- sapply(k_values, within_cluster_sum_of_squares)

  # Plot the elbow curve
  elbow_plot <- ggplot() +
    geom_line(aes(x = k_values, y = withiness_for_k), color = "blue") +
    geom_point(aes(x = k_values, y = withiness_for_k), color = "red") +
    labs(title = "Elbow Method for Optimal K", x = "Number of Clusters (K)", y = "Total Within-Cluster Sum of Squares") 

    print(elbow_plot)


  #As we can see the sum of square distance for k=10 is minimum which means dataset can be divided into 10 neighbourhoods.

  kmeans_result <- kmeans(scaled_data, centers = 10)

  # Creating a new variable called neighbourhood_cluster and assigning every listing the cluster number which it belongs to.
  cleaned_listings_data$neighbourhood_cluster <- kmeans_result$cluster
  cluster_assignments <- kmeans_result$cluster
  cluster_centers <- kmeans_result$centers

  #Plotting the geography of the listings
  ggplot(data = neighbourhood_data, aes(x = latitude, y = longitude, color = as.factor(cluster_assignments))) +
    geom_point() +
    labs(title = "K-Means Clustering", color = "Cluster") +
    theme_minimal()

# Re-leveling the variable neigbourhood to remove inconsistent values and creating a new vairable named neigbourhood_cleaned.
cleaned_listings_data <- cleaned_listings_data %>%
  mutate(neighbourhood_cleaned = case_when(
    # Case-insensitive pattern matching
    str_detect(neighbourhood, regex("Old Toronto", ignore_case = TRUE)) ~ "Old Toronto",
    str_detect(neighbourhood, regex("West Toronto", ignore_case = TRUE)) ~ "West Toronto",
    str_detect(neighbourhood, regex("Toronto", ignore_case = TRUE)) ~ "Toronto",
    str_detect(neighbourhood, regex("North York", ignore_case = TRUE)) ~ "North York",
    str_detect(neighbourhood, regex("East York", ignore_case = TRUE)) ~ "East York",
    str_detect(neighbourhood, regex("York", ignore_case = TRUE)) ~ "York",
    str_detect(neighbourhood, regex("Etobicoke", ignore_case = TRUE)) ~ "Etobicoke",
    str_detect(neighbourhood, regex("Vaughan", ignore_case = TRUE)) ~ "Vaughan",
    str_detect(neighbourhood, regex("West Toronto", ignore_case = TRUE)) ~ "West Toronto",
    str_detect(neighbourhood, regex("Scarborough", ignore_case = TRUE)) ~ "Scarborough",
    str_detect(neighbourhood, regex("Markham", ignore_case = TRUE)) ~ "Markham",
    TRUE ~ "Other"))


#Thus dropping the variables which no longer are required.
cleaned_listings_data <- select(cleaned_listings_data,-latitude,-longitude,-neighbourhood_cleansed,-neighbourhood)

#Number of listings per neighborhood
summary <- cleaned_listings_data %>% group_by(neighbourhood_cluster) %>% summarise(Count=n())
ggplot(data = summary , aes(x = neighbourhood_cluster, y = Count))  + 
  geom_bar(stat="identity", position = "dodge") +  ggtitle("Number of listings per neighborhood cluster")


#Analyzing prices with neigbourhood location
summary <- cleaned_listings_data %>% group_by(neighbourhood_cleaned) %>% summarise(mean_prices=mean(price))
ggplot(data = summary , aes(x = neighbourhood_cleaned, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge")  +
  ggtitle("Price Vs Neighbourhood") 

#Analyzing prices with neigbourhood cluster
summary <- cleaned_listings_data %>% group_by(neighbourhood_cluster) %>% summarise(mean_prices=mean(price))
ggplot(data = summary , aes(x = neighbourhood_cluster, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge")  +
  ggtitle("Price Vs Neighbourhood")


# Analysing property types
unique(cleaned_listings_data$property_type)

#Creating a new variable named property_category and categorising property_type into shared, private and other type.

cleaned_listings_data <- cleaned_listings_data %>%
  mutate(property_category = case_when(
    str_detect(property_type, "Entire") ~ "Entire property",
    str_detect(property_type, "Private") ~ "Private Room",
    str_detect(property_type, "Shared") ~ "Shared Room",
    TRUE ~ "Other"))


#Analysing the property_type distribution of the listings.
property_type_count <- table(cleaned_listings_data$property_category)

# Create a data frame for ggplot
df <- data.frame( Property_type = names(property_type_count), Count = property_type_count)

# Plot the pie chart
ggplot(df, aes(x = "", y = Count.Freq, fill = Property_type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void() +
  ggtitle("Distribution of Airbnb Listings by Room Type")

cleaned_listings_data <- select(cleaned_listings_data,-property_type)


#Analysing the count of property types.
summary_category <- cleaned_listings_data %>% group_by(property_category) %>% summarise(count=n())
ggplot(data = summary_category , aes(x = property_category, y = count))  + 
  geom_bar(stat="identity", position = "dodge")

#Analysing the prices per property type.
summary <- cleaned_listings_data %>% group_by(property_category) %>% summarise(mean_prices=mean(price))
ggplot(data = summary , aes(x = property_category, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge") + ggtitle("Price Vs Property_Category")


#Analysing amenities and dividing into different categories
array_of_arrays <- list(
  list("wifi", "TV"),
  list("Air conditioning", "Central air conditioning","Heating","Gym","Excerise euipement","Extra pillows and blankets",
       "patio or balcony","private entrance","private backyard"),
  list("toaster","rice maker","hot water kettle","stove","coffee maker","oven","mircrowave","refrigerator"),
  list("carbon monoxide alarm","fire extinguisher","smoke alarm","first aid kit"),
  list("shampoo","conditioner","hair dryer","hot tub","shower gel","bath tub"),
  list("parking"),
  list("long term stays allowed")
)

# Function to check if any key is present in the observation
check_keys <- function(keys, observation) {
  any(sapply(keys, function(key) grepl(key, observation, ignore.case = TRUE)))
}

# Iterate over the array of arrays and check keys in each observation
amenties_results <- sapply(array_of_arrays, function(keys) {
  sapply(cleaned_listings_data$amenities, function(observation) {
    check_keys(keys, observation)
  })
})

# Convert the matrix to a data frame
amenities <- as.data.frame(amenties_results)
colnames(amenities) <- c("basics", "facilities", "kitchen_appliances","safety_measures","bath_essentials","parking","long_term_stays_allowed")
# Print the result
rownames(amenities) <- NULL
head(amenities) %>% View()

#Combine these columns with our existing dataframe.
cleaned_listings_data <- cbind(cleaned_listings_data,amenities)
#cleaned_listings_data <- select(cleaned_listings_data,-amenities)
head(cleaned_listings_data) %>% view()
count_true_values <- colSums(amenities)

# Analysing the presence of amenities in all the listings.
barplot(count_true_values, names.arg = colnames(amenities), col = "skyblue", main = "Analysing various amenities", ylab = "Count", xlab = "Columns") + theme_minimal()
cleaned_listings_data <- select(cleaned_listings_data,-amenities)


#Factorizing the categorical variables

columns_to_convert <- c("host_is_superhost","has_availability","instant_bookable","basics","facilities","kitchen_appliances",
                        "safety_measures","bath_essentials","parking","long_term_stays_allowed","host_response_rate",
                        "host_acceptance_rate","host_response_time","host_identity_verified","room_type","neighbourhood_cleaned","property_category","bathrooms_text",
                        "host_verifications")

cleaned_listings_data[columns_to_convert] <- lapply(cleaned_listings_data[columns_to_convert], as.factor)

#Analysing the price distribution

summary(cleaned_listings_data$price)
sum(cleaned_listings_data$price <=500)/length(cleaned_listings_data$price)

cleaned_listings_data %>% filter(price <=500) %>% ggplot(aes(price))+
  geom_density(fill = "deepskyblue", size = 1.5, color = "navyblue", alpha = 0.5)+
  xlab("Price")+
  ylab("Density")+
  ggtitle("Price Distribution of the listings")+
  theme(plot.title = element_text(hjust = 0.5))

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
summary <- cleaned_listings_data %>% group_by(accommodates) %>% summarise(mean_prices=mean(price))
ggplot(data = summary , aes(x = accommodates, y = mean_prices))  + 
  geom_bar(stat="identity", position = "dodge") +  ggtitle("Price Vs Accommodates")



#Saving this dataset.
write.csv(cleaned_listings_data, file = "C:/Users/KRISHNA/Desktop/SHWETA/Fall 2023/BA/Project/Price prediction/Dataset/Cleaned_listings_scored_data.csv", row.names = FALSE)

........................................................... 
#Put your sentimental analysis code over here.




#Merging the emotional scores for every listing with the pre-processed data.

clean_listings_scored <- cleaned_listings_data %>% 
  inner_join(sentiment_score, by = c("id"="listing_id"))

#Reviewing the final data which will be used for model building.
str(clean_listings_scored)
colnames(clean_listings_scored)


#Analysing the significance of features on the price using Random Forest.

model <- randomForest(price ~ . ,data = clean_listings_scored)
summary(model)
# Display variable importance
print(importance(model))
# Plot variable importance
importance_df <- as.data.frame(importance(model))
# Extract variable importance
importance_df <- data.frame(
  Variable = rownames(importance_df),
  Importance = importance_df$IncNodePurity
)
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
view(importance_df)

library(ggplot2)
ggplot(importance_df, aes(x = Importance, y = reorder(Variable, Importance))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Importance (Regression) against Price", x = "IncNodePurity", y = "Variables") +
  theme(axis.text.y = element_text(vjust = 0.5, hjust=1))


#Model building code.................................


#After this put the host analytics code....


#The End....
















