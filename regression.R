# ---------------------------------------------------------
# Charger les bibliothèques nécessaires
# ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------
# 1) Lire le fichier CSV
# ---------------------------------------------------------
data <- read.csv("Online_Retail_Clean_Enhanced.csv")

# ---------------------------------------------------------
# 2) Graphiques exploratoires
# ---------------------------------------------------------
# 2a) Nuage de points (Quantity vs. TotalAmount) avec ligne de régression
scatter_plot <- ggplot(data, aes(x = Quantity, y = TotalAmount)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Quantity vs Total Amount",
       x = "Quantity",
       y = "Total Amount") +
  theme_minimal()

# 2b) Histogrammes
hist_amount <- ggplot(data, aes(x = TotalAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Total Amount",
       x = "Total Amount",
       y = "Frequency") +
  theme_minimal()

hist_quantity <- ggplot(data, aes(x = Quantity)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Quantity",
       x = "Quantity",
       y = "Frequency") +
  theme_minimal()

# 2c) Graphe Q-Q
qq_plot <- ggplot(data, aes(sample = TotalAmount)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Total Amount",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# ---------------------------------------------------------
# 3) Analyse statistique
# ---------------------------------------------------------
# 3a) Test de corrélation
correlation <- cor.test(data$Quantity, data$TotalAmount)

# ---------------------------------------------------------
# 4) Afficher les résultats dans la console
# ---------------------------------------------------------
cat("\nCorrelation Analysis:")
print(correlation)

# ---------------------------------------------------------
# 5) Afficher les graphiques
# ---------------------------------------------------------
scatter_plot
hist_amount
hist_quantity
qq_plot
