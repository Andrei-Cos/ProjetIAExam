# ===================== Comparaison des deux approches PCA =====================

library(FactoMineR)
library(factoextra)
library(dplyr)

mydata <- read.csv("Online_Retail_Clean_Enhanced.csv", header = TRUE, stringsAsFactors = FALSE)


# Première approche : Variables originales uniquement
original_vars <- mydata %>% select(Quantity, UnitPrice)
pca_original <- PCA(original_vars, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Deuxième approche : Avec variables dérivées
derived_vars <- mydata %>%
  select(Quantity, UnitPrice, TotalAmount, ItemsPerTransaction)
pca_derived <- PCA(derived_vars, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Comparaison des variances expliquées
par(mfrow = c(1, 2))

# Variance expliquée - Variables originales
barplot(pca_original$eig[, 2], 
        main = "Variance expliquée\n(Variables originales)", 
        names.arg = paste("Dim", 1:2),
        col = "lightblue",
        ylab = "Pourcentage de variance")

# Variance expliquée - Variables dérivées
barplot(pca_derived$eig[, 2], 
        main = "Variance expliquée\n(Variables dérivées)", 
        names.arg = paste("Dim", 1:4),
        col = "lightgreen",
        ylab = "Pourcentage de variance")

par(mfrow = c(1, 1))

# Afficher les contributions des variables pour chaque approche
cat("\nContributions des variables (Approche originale):\n")
print(round(pca_original$var$contrib, 2))

cat("\nContributions des variables (Approche avec variables dérivées):\n")
print(round(pca_derived$var$contrib, 2))

# Analyse des corrélations entre les nouvelles variables
correlation_matrix <- cor(derived_vars)
print("\nMatrice de corrélation des variables dérivées:")
print(round(correlation_matrix, 2))

# Créer une visualisation des patterns temporels
temporal_patterns <- mydata %>%
  group_by(Hour) %>%
  summarise(
    avg_quantity = mean(Quantity),
    avg_amount = mean(TotalAmount),
    n_transactions = n()
  )

# Visualiser les patterns horaires
par(mfrow = c(2, 1))
plot(temporal_patterns$Hour, temporal_patterns$n_transactions,
     type = "l", col = "blue",
     main = "Nombre de transactions par heure",
     xlab = "Heure", ylab = "Nombre de transactions")

plot(temporal_patterns$Hour, temporal_patterns$avg_amount,
     type = "l", col = "red",
     main = "Montant moyen des transactions par heure",
     xlab = "Heure", ylab = "Montant moyen")
par(mfrow = c(1, 1))

