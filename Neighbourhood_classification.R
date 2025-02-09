library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
#read calendar data
listings_data <- read.csv("C:/Users/KRISHNA/Desktop/listings.csv")


neighbourhood_data <- select(listings_data, latitude, longitude)
scaled_data <- scale(neighbourhood_data)
k <- 10  # Choose the number of clusters
kmeans_result <- kmeans(scaled_data, centers = k)
neighbourhood_data$neighbourhood_cluster <- kmeans_result$cluster
cluster_assignments <- kmeans_result$cluster
cluster_centers <- kmeans_result$centers
# Assuming your_data has two features (replace with actual column names)
ggplot(data = neighbourhood_data, aes(x = latitude, y = longitude, color = as.factor(cluster_assignments))) +
  geom_point() +
  labs(title = "K-Means Clustering", color = "Cluster") +
  theme_minimal()

neighbourhood_data


#To determine optimal value of K
scaled_data <- scale(data)

# Function to calculate total within-cluster sum of squares
wss <- function(k) {
  kmeans_result <- kmeans(scaled_data, centers = k)
  return(sum(kmeans_result$withinss))
}

# Try different values of k and calculate the total within-cluster sum of squares
k_values <- 1:10  # Try different values of k, adjust as needed
total_withinss <- sapply(k_values, wss)

# Plot the elbow curve
elbow_plot <- ggplot() +
  geom_line(aes(x = k_values, y = total_withinss), color = "blue") +
  geom_point(aes(x = k_values, y = total_withinss), color = "red") +
  labs(title = "Elbow Method for Optimal K", x = "Number of Clusters (K)", y = "Total Within-Cluster Sum of Squares") +
  theme_minimal()

print(elbow_plot)
