# Airbnb_data_analytics

## Project Overview
This project aims to analyze Airbnb listings in Toronto to determine the key factors affecting pricing. It leverages data analytics and machine learning techniques, including regression analysis and sentiment analysis, to provide actionable insights for Airbnb hosts.

## Business Case
Setting the right price is crucial for maximizing bookings and revenue. If a listing is priced too high, it may receive fewer bookings, while underpricing can lead to lost profit. This project uses data-driven methods to predict pricing and help hosts optimize their listings. Additionally, sentiment analysis of customer reviews provides insights into guest satisfaction and areas for improvement.

## Dataset
- **Source:** [InsideAirbnb.com](http://insideairbnb.com)
- **Listings Dataset:** 18,921 observations with 75 features, containing information about hosts, properties, and review scores.
- **Reviews Dataset:** 482,000 observations with 6 features, providing customer feedback for each listing.

## Data Processing
### 1. Handling Missing Values
- Eliminated variables with excessive null values (e.g., `bathrooms`).
- Replaced missing values in key features (e.g., `review_scores_rating`) with `0`.

### 2. Feature Selection & Engineering
- Removed unimportant features (e.g., `listing_url`, `host_name`).
- **Outlier removal:** Prices above `$500` were excluded.
- **Releveled categorical data:** `property_type` simplified to major categories.
- **Created new categorical features** from `amenities` (e.g., `kitchen_appliances`, `safety_measures`).

### 3. Clustering
- Grouped listings based on `latitude` and `longitude` using **KNN clustering**.

## Exploratory Data Analysis (EDA)
- Visualized **property type distribution**.
- Categorized **host performance** based on review scores (`average`, `excellent`, `poor`).
- Performed **emotional analysis** of customer reviews using **NRC Lexicon**.

## Machine Learning Models
Two models were built and compared:
1. **Linear Regression**
2. **Random Forest**

- Feature selection was based on `IncNodePurity` from Random Forest.
- **5-Fold Cross-Validation** was used for performance evaluation.
- **Sentiment analysis** was incorporated into pricing predictions.

## Key Findings
- Certain **property features and amenities** significantly impact pricing.
- **Host behavior metrics** (e.g., response rate, acceptance rate) influence customer satisfaction.
- **Sentiment analysis** helps assess the **emotional tone** of customer reviews, providing insights for hosts to improve their services.
- The model provides an estimated **price proposal** for new listings.

## Technologies Used
- **Programming Languages:** R
- **Libraries:** ggplot2, dplyr, tidyr, caret,
- **Machine Learning Techniques:** Linear Regression, Random Forest, Sentiment Analysis

## Conclusion
This project equips Airbnb hosts with data-driven insights to optimize pricing strategies and enhance customer satisfaction. The findings can help hosts make informed decisions to maximize bookings and revenue.



