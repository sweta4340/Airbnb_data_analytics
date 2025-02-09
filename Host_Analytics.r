library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)


#Reading the listings data.
listings_data <- read.csv("C:/Users/KRISHNA/Desktop/listings.csv")
#view(listings_data)

head(listings_data)

colnames(listings_data)


#Selecting the host information available for the dataset.
host_data <- select(listings_data,"id","host_id","host_name","host_since","host_response_time","host_response_rate","host_acceptance_rate","host_is_superhost",
                    "host_verifications","host_identity_verified","calculated_host_listings_count",
                    "calculated_host_listings_count_entire_homes","calculated_host_listings_count_private_rooms",
                    "calculated_host_listings_count_shared_rooms") 

host_data

#Checking the number of null values for each column
colSums(is.na(host_data))

# checking the number of rows having atleast one null value
sum(is.na(host_data))

# removing the rows with 'N/A' values in column 'host_response_time'
host_data <- subset(host_data, host_response_time!="N/A")
host_data <- subset(host_data,host_response_rate!="N/A")
host_data



#Selecting the customer reviews information form the available dataset.
reviews_data <- select(listings_data,"id","host_id","number_of_reviews","number_of_reviews_ltm","number_of_reviews_l30d",
                  "first_review","last_review","review_scores_rating","review_scores_accuracy","review_scores_cleanliness",
                  "review_scores_checkin","review_scores_communication","review_scores_location",
                  "review_scores_value")
reviews_data

str(reviews_data)

#replace NA values with 0
reviews_data <- reviews_data %>% 
  mutate(review_scores_rating = coalesce(review_scores_rating, 0),
                                         coalesce(review_scores_cleanliness, 0),
                                        coalesce(review_scores_checkin, 0),
                                        coalesce(review_scores_communication, 0),
                                        coalesce(review_scores_location, 0))

#summarizing the various average review scores for all the listings for every host.
summary <- reviews_data %>%
   group_by(host_id) %>%
           summarize(Count=n(),
                     Avg_review_score_rating=mean(review_scores_rating,na.rm = TRUE),
                     Avg_review_score_cleanliness=mean(review_scores_cleanliness,na.rm = TRUE),
                     Avg_review_score_checkin=mean(review_scores_checkin,na.rm = TRUE),
                     Avg_review_score_communication=mean(review_scores_communication,na.rm = TRUE),
                     Avg_review_score_location=mean(review_scores_location,na.rm = TRUE))
head(summary)

# unique values for the number of listings each host is having
unique(summary$Count)

#dropping the rows with atleast one NaN values
summary <- drop_na(summary)

summary <- summary %>% mutate(Overall_avg_ratings=
                              (Avg_review_score_rating+Avg_review_score_cleanliness+Avg_review_score_checkin+Avg_review_score_communication+
                              Avg_review_score_location)/5)
head(summary)

unique(summary$Overall_avg_ratings)

#Getting the host_ids with Overall_avg_ratings greater than 4
host_ids_with_excellent_ratings <- filter(summary,Overall_avg_ratings>4) %>% select(host_id)
#Getting the host_ids with Overall_avg_ratings greater than 3 and less than 4
host_ids_with_average_ratings <- filter(summary,Overall_avg_ratings>3 & Overall_avg_ratings<=4) %>% select(host_id)
#Getting the host_ids with Overall_avg_ratings less than 3
host_ids_with_poor_ratings <- filter(summary,Overall_avg_ratings<=3) %>% select(host_id)


#Creating a new variable named host_performance and assigning every host with either Excellent, Average and Poor categories based on the ratings
# they received from customer.
host_data <- host_data %>% mutate(host_performance = if_else(host_id %in% host_ids_with_excellent_ratings$host_id,"Excellent",
                                        if_else(host_id %in% host_ids_with_average_ratings$host_id,"Average","Poor")))
host_data

#Analysing host_performance with their response time.
data <- host_data %>% group_by(host_performance,host_response_time) %>% summarise(Count=n())
data <- subset(data, host_response_time!="N/A" & host_response_time!="" )
ggplot(data=data, aes(x=host_performance, y=Count, fill=host_response_time)) + geom_bar(stat="identity", position = "dodge") +
 ggtitle("host response time Vs host performance")  +  theme(
  plot.margin = margin(1, 1, 1, 1, "cm")
)


#Analysing host_performance with their identity verification.
data <- host_data %>% group_by(host_performance,host_identity_verified) %>% summarise(Count=n())
data <- subset(data,host_identity_verified!="")
ggplot(data=data, aes(x=host_performance, y=Count, fill=host_identity_verified)) + geom_bar(stat="identity", position = "dodge") +
 ggtitle("count of verified hosts Vs host performance")  +  theme(
  plot.margin = margin(1, 1, 1, 1, "cm")
)

#Analysing host_performance with their superhost status.
data <- host_data %>% group_by(host_performance,host_is_superhost) %>% summarise(Count=n())
data <- subset(data,host_is_superhost!="")
ggplot(data=data, aes(x=host_performance, y=Count, fill=host_is_superhost)) + geom_bar(stat="identity", position = "dodge") +
 ggtitle("count of superhosts Vs host performance") +  theme(
  plot.margin = margin(1, 1, 2, 2, "cm")
)

#Categorizing the variable host_acceptance rate into >75%, 50%-75%, 25%-50%a and <25% for better understanding.
data <- select(host_data,host_performance,host_acceptance_rate)
data <- subset(data,host_acceptance_rate!="NA")
data$host_acceptance_rate <- as.numeric(sub("%", "",data$host_acceptance_rate))
data <- data %>% mutate("host_acceptance_range"= if_else(host_acceptance_rate>=75,">75%",
                                      if_else(host_acceptance_rate<75 & host_acceptance_rate>50,"50%-75%",
                                      if_else(host_acceptance_rate<=50 & host_acceptance_rate>25,"25%-50%","<25%"))))
data <- subset(data,host_acceptance_range!="NA")
data <- data %>% group_by(host_performance,host_acceptance_range) %>% summarise(Count=n())

#Analysing the host_performance with their acceptance rate.
ggplot(data=data, aes(x=host_performance, y=Count, fill=host_acceptance_range)) + geom_bar(stat="identity", position = "dodge") +
 ggtitle("host acceptance rate Vs host performance")  +  theme(
  plot.margin = margin(1, 1, 1, 1, "cm")
)

#Categorizing the variable host_response rate into >75%, 50%-75%, 25%-50%a and <25% for better understanding.
data <- select(host_data,host_performance,host_response_rate)
data <- subset(data,host_response_rate!="N/A")
data$host_response_rate <- as.numeric(sub("%", "",data$host_response_rate))
data <- data %>% mutate("host_response_range"= if_else(host_response_rate>=75,">75%",
                                      if_else(host_response_rate<75 & host_response_rate>50,"50%-75%",
                                      if_else(host_response_rate<=50 & as.numeric(host_response_rate)>25,"25%-50%","<25%"))))
data <- data %>% group_by(host_performance,host_response_range) %>% summarise(Count=n())
data <- subset(data,host_response_range!="NA")

#Analysing the host_performance with their response rate.
ggplot(data=data, aes(x=host_performance, y=Count, fill=host_response_range)) + geom_bar(stat="identity", position = "dodge") +
 ggtitle("host response rate Vs host performance")  +  theme(
  plot.margin = margin(1, 1, 1, 1, "cm")
)

