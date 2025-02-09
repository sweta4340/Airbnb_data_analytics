library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(tidyverse)

set.seed(1)
fold <- c(1:10)
fold_v <- sample(fold, nrow(cleaned_listings_data), replace=TRUE)
fold_v


str(cleaned_listings_data)


testing_RSE_model <- double(10)
training_RSE_model <- double(10)

my_RSE <- function(y, y_hat){
  n = length(y)
  diff = y - y_hat
  RSE = sqrt(1/(n-2) * sum(diff^2))
  return(RSE)
}

for (counter in c(1:10)){
  
  train_set <- cleaned_listings_data[fold_v != counter,]
  test_set <- cleaned_listings_data[fold_v == counter,]
  
  
  
  train_set$bathrooms_text <- factor(train_set$bathrooms_text, levels = unique(c(levels(train_set$bathrooms_text),levels(test_set$bathrooms_text))))
  train_set$host_response_time <- factor(train_set$host_response_time, levels = unique(c(levels(train_set$host_response_time), levels(test_set$host_response_time))))
  train_set$host_response_rate <- factor(train_set$host_response_rate, levels = unique(c(levels(train_set$host_response_rate), levels(test_set$host_response_rate))))
  train_set$host_acceptance_rate <- factor(train_set$host_acceptance_rate, levels = unique(c(levels(train_set$host_acceptance_rate), levels(test_set$host_acceptance_rate))))
  train_set$room_type  <- factor(train_set$room_type, levels = unique(c(levels(train_set$room_type), levels(test_set$room_type))))
  train_set$neighbourhood_cleaned <- factor(train_set$neighbourhood_cleaned, levels = unique(c(levels(train_set$neighbourhood_cleaned), levels(test_set$neighbourhood_cleaned))))
  train_set$host_verifications <- factor(train_set$host_verifications, levels = unique(c(levels(train_set$host_verifications), levels(test_set$host_verifications))))
  
  model <- lm(price ~ . ,data=train_set)
  model.test.pred <- predict(model, test_set)
  model.train.pred <- predict(model,train_set)
  testing_RSE_model[counter] <- my_RSE(test_set$price,model.test.pred)
  training_RSE_model[counter] <- my_RSE(train_set$price,model.train.pred)
  
}

testing_RSE_model







