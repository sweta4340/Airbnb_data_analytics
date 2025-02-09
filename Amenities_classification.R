library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
#read calendar data
listings_data <- read.csv("C:/Users/KRISHNA/Desktop/listings.csv")

unique(listings_data$amenities)

# Array of arrays (list of lists) containing keys
array_of_arrays <- list(
  list("wifi", "TV"),
  list("Air conditioning", "Central air conditioning","Heating","Gym","Excerise euipement","Extra pillows and blankets",
        "patio or balcony","private entrance","private backyard"),
  list("toaster","rice maker","hot water kettle","stove","coffee maker","oven","mircrowave","refrigerator"),
  list("carbon monoxide alarm","fire extinguisher","smoke alarm","first aid kit"),
  list("shampoo","conditioner","hair dryer","hot tub","shower gel","bath tub"),
  list("parking"),
  list("long term stayes allowed")
)

# Function to check if any key is present in the observation
check_keys <- function(keys, observation) {
  any(sapply(keys, function(key) grepl(key, observation, ignore.case = TRUE)))
}

# Iterate over the array of arrays and check keys in each observation
amenties_results <- sapply(array_of_arrays, function(keys) {
  sapply(listings_data$amenities, function(observation) {
    check_keys(keys, observation)
  })
})



# Convert the matrix to a data frame
amenities <- as.data.frame(amenties_results)
colnames(amenities) <- c("Basics", "Facilities", "Kitchen Appliances","Safety Measures","Bath Essentials","Parking","Long term stayes allowed")
# Print the result
rownames(amenities) <- NULL
head(amenities) %>% View()

count_true_values <- colSums(amenities)

# Plot a bar graph
barplot(count_true_values, names.arg = colnames(amenities), col = "skyblue", main = "Analysing various amenities", ylab = "Count", xlab = "Columns") + theme_minimal()




