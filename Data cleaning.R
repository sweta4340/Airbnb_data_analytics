library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)

#Reading the listings data
listings_data <- read.csv("C:/Users/KRISHNA/Desktop/listings.csv")

# Analyzing variables in the dataset.
str(listings_data)

#Understanding the column names.
colnames(listings_data)

#Dropping features which doesn't have any affect our pricing prediction analysis.
listings_data <- subset(listings_data, select=c("id",'host_id','host_response_time', 'host_response_rate', 'host_acceptance_rate', 'host_is_superhost',
                                                'host_listings_count', 'host_total_listings_count', 'host_verifications',
                                                'host_identity_verified','neighbourhood', 'neighbourhood_cleansed',
                                                'latitude','longitude','property_type','room_type','accommodates', 'bathrooms_text',
                                                'bedrooms','beds', 'amenities', 'price', 'minimum_nights', 'maximum_nights', 'minimum_minimum_nights',
                                                'maximum_minimum_nights', 'minimum_maximum_nights', 'maximum_maximum_nights', 'minimum_nights_avg_ntm',
                                                'maximum_nights_avg_ntm', 'has_availability','availability_30', 'availability_60',
                                                'availability_90', 'availability_365', 'number_of_reviews',
                                                'number_of_reviews_ltm' ,'number_of_reviews_l30d',
                                                'review_scores_rating', 'review_scores_accuracy', 'review_scores_cleanliness', 'review_scores_checkin',
                                                'review_scores_communication', 'review_scores_location', 'review_scores_value',
                                                'instant_bookable', 'calculated_host_listings_count', 'calculated_host_listings_count_entire_homes',
                                                'calculated_host_listings_count_private_rooms', 'calculated_host_listings_count_shared_rooms',
 
                                                                                               'reviews_per_month'))
#Converting char price into numeric value.
listings_data$price <- as.numeric(gsub('[$,]', '', listings_data$price))

#Determining number of null values in every column.
colSums(is.na(listings_data))

#Replacing null vales with 0 for some columns.
unique(listings_data$bedrooms)
listings_data <- listings_data %>% 
  mutate(bedrooms = coalesce(bedrooms, 0))
listings_data <- listings_data %>% 
  mutate(reviews_per_month = coalesce(reviews_per_month, 0))
listings_data <- listings_data %>% 
  mutate(beds = coalesce(beds, 0))


#Dropping NA values from this columns since they are less in number.
listings_data <- listings_data %>% drop_na(host_listings_count)
listings_data <- listings_data %>% drop_na(host_total_listings_count)
listings_data <- listings_data %>% drop_na(bathrooms_text)
colSums(is.na(listings_data))

#Replacing NA values with 0 in review_scores data
listings_data <- listings_data %>% 
  mutate(review_scores_rating = coalesce(review_scores_rating, 0),
         review_scores_accuracy = coalesce(review_scores_accuracy,0),
         review_scores_cleanliness=coalesce(review_scores_cleanliness, 0),
         review_scores_checkin=coalesce(review_scores_checkin, 0),
         review_scores_communication=coalesce(review_scores_communication, 0),
         review_scores_location=coalesce(review_scores_location, 0),
         review_scores_value=coalesce(review_scores_value,0))

#Now our data does not have any null values
colSums(is.na(listings_data))

#saving the file
write.csv(listings_data, file = "C:/Users/KRISHNA/Desktop/SHWETA/Fall 2023/BA/Project/Price prediction/Dataset/Cleaned_Listings_data.csv", row.names = FALSE)
