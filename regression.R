# ---------------------------------------------------------
# Charger les library 
# ---------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# ---------------------------------------------------------
# 1) Lire le fichier CSV
# ---------------------------------------------------------
data <- read.csv("Online_Retail_Clean_Enhanced.csv")

# ---------------------------------------------------------
# 2) Transformation logarithmique des variables
# ---------------------------------------------------------
# Ajouter une petite constante pour éviter log(0)
data <- data %>%
  mutate(
    Log_TotalAmount = log1p(TotalAmount),  # log(1 + x)
    Log_Quantity = log1p(Quantity)
  )

# ---------------------------------------------------------
# 3) Graphiques exploratoires après transformation
# ---------------------------------------------------------
# 3a) Nuage de points (Log_Quantity vs. Log_TotalAmount) avec ligne de régression
scatter_plot <- ggplot(data, aes(x = Log_Quantity, y = Log_TotalAmount)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log(Quantity) vs Log(Total Amount)",
       x = "Log(Quantity)",
       y = "Log(Total Amount)") +
  theme_minimal()

# 3b) Histogrammes
hist_log_amount <- ggplot(data, aes(x = Log_TotalAmount)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Log(Total Amount)",
       x = "Log(Total Amount)",
       y = "Frequency") +
  theme_minimal()

hist_log_quantity <- ggplot(data, aes(x = Log_Quantity)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Log(Quantity)",
       x = "Log(Quantity)",
       y = "Frequency") +
  theme_minimal()

# 3c)  Q-Q Plot 
qq_plot_log <- ggplot(data, aes(sample = Log_TotalAmount)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Q-Q Plot: Log(Total Amount)",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()

# ---------------------------------------------------------
# 4) Analyse statistique
# ---------------------------------------------------------

correlation_log <- cor.test(data$Log_Quantity, data$Log_TotalAmount)

# ---------------------------------------------------------
# 5) Afficher les résultats dans la console
# ---------------------------------------------------------
cat("\nCorrelation Analysis (Log-Transformed):")
print(correlation_log)

# ---------------------------------------------------------
# 6) Afficher les graphiques
# ---------------------------------------------------------
scatter_plot
hist_log_amount
hist_log_quantity
qq_plot_log

