# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("Online_Retail_Clean_Enhanced.csv")

# Create a scatterplot of Quantity vs TotalAmount to check for linear relationship
plot1 <- ggplot(data, aes(x = Quantity, y = TotalAmount)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "Scatter Plot: Quantity vs Total Amount",
       x = "Quantity",
       y = "Total Amount") +
  theme(plot.title = element_text(hjust = 0.5))

# Create histograms for key numerical variables
plot2 <- ggplot(data, aes(x = TotalAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Total Amount",
       x = "Total Amount",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

plot3 <- ggplot(data, aes(x = Quantity)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of Quantity",
       x = "Quantity",
       y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Create Q-Q plots to check for normality
plot4 <- ggplot(data, aes(sample = TotalAmount)) +
  stat_qq() +
  stat_qq_line() +
  theme_minimal() +
  labs(title = "Q-Q Plot: Total Amount",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme(plot.title = element_text(hjust = 0.5))

# Calculate correlation coefficient
correlation <- cor.test(data$Quantity, data$TotalAmount)

# Save the plots
pdf("sales_analysis_plots.pdf")
print(plot1)
print(plot2)
print(plot3)
print(plot4)
dev.off()



# Print statistical results
cat("\nCorrelation Analysis:")
print(correlation)




