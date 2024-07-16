library(tidyverse)
library(cluster)

#Read data from file
setwd("C:/Users/User/Documents/Predictive Modeling/")
data <- read.csv("assignment/e-shop_clothing 2008.csv", header = TRUE)


# Convert 'price2' to numeric if it's not already, assuming 'yes' is 1 and 'no' is 2
data$price.2 <- ifelse(data$price.2 == 'yes', 1, 2)

# Extract only the 'price' column for clustering (since 'price2' is binary and may not work well with k-means)
X <- data.frame(price = data$price)

# Determine the optimal number of clusters using the Elbow method
set.seed(123) 
total.wss <- numeric(15)
for (k in 1:15) {
  km <- kmeans(X, centers=k, nstart=25)
  total.wss[k] <- km$tot.withinss
}

# Plot the Elbow method graph
plot(1:15, total.wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares", main="Elbow Method")

# Perform k-means clustering on the non-scaled 'price' data
# Assuming from the Elbow plot that the optimal number of clusters is 4
set.seed(123)
km.out <- kmeans(X, 4, nstart=20)

# Add the cluster assignments to the original dataframe
data$cluster <- km.out$cluster

# Plot the clusters based on 'price'
ggplot(data, aes(x = price, y = as.factor(cluster), color = as.factor(cluster))) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
  scale_color_manual(values = rainbow(4)) +
  labs(title = "k-Means Clustering of Prices",
       x = "Price (USD)",
       y = "Cluster Assignment") +
  theme_minimal() +
  theme(legend.position = "bottom")


# Perform k-means clustering on both 'price' and 'price2'.
# Note: Since 'price2' is binary, consider the impact this may have on the clustering result.
set.seed(123)
km.out <- kmeans(X, 2, nstart=20)

# Add the cluster assignments to the original dataframe
data$cluster <- km.out$cluster

# Plot the clusters based on 'price.2'
ggplot(data, aes(x = price, y = as.factor(cluster), color = as.factor(cluster))) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
  scale_color_manual(values = rainbow(4)) +
  labs(title = "k-Means Clustering of Prices",
       x = "Price (USD)",
       y = "Cluster Assignment") +
  theme_minimal() +
  theme(legend.position = "bottom")


