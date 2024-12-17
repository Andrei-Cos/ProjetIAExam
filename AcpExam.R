# ===================== Étape 1 : Préparation des données =====================

# Importer le fichier nettoyé
mydata <- read.csv("Online_Retail_Clean.csv", header = TRUE, stringsAsFactors = FALSE)

# Sélectionner uniquement les variables numériques pertinentes
numerical_data <- mydata[, c("Quantity", "UnitPrice")]

# ===================== Étape 2 : Boxplots avec valeurs aberrantes =====================

# Afficher les boxplots avec les outliers visibles
par(mfrow = c(1, 2)) # Deux graphiques côte à côte
boxplot(numerical_data$Quantity, 
        main = "Boxplot - Quantity (avec outliers)", 
        col = "lightblue", ylab = "Quantité")

boxplot(numerical_data$UnitPrice, 
        main = "Boxplot - UnitPrice (avec outliers)", 
        col = "lightgreen", ylab = "Prix Unitaire")

par(mfrow = c(1, 1)) # Retour à un affichage normal

# ===================== Étape 3 : Boxplots sans valeurs aberrantes =====================

# Afficher les boxplots sans les outliers (outline = FALSE)
par(mfrow = c(1, 2)) # Deux graphiques côte à côte
boxplot(numerical_data$Quantity, 
        main = "Boxplot - Quantity (sans outliers)", 
        col = "lightblue", ylab = "Quantité", outline = FALSE)

boxplot(numerical_data$UnitPrice, 
        main = "Boxplot - UnitPrice (sans outliers)", 
        col = "lightgreen", ylab = "Prix Unitaire", outline = FALSE)

par(mfrow = c(1, 1)) # Retour à un affichage normal

# ===================== Étape 4 : Application de l'ACP =====================

# Centrage et réduction des données
scaled_data <- scale(numerical_data)

# Charger les packages nécessaires
library(FactoMineR)   # Pour l'ACP
library(factoextra)   # Pour les visualisations

# Appliquer l'ACP
resultats_ACP <- PCA(scaled_data, scale.unit = TRUE, ncp = 2, graph = FALSE)

# ===================== Étape 5 : Habillage des individus par régions =====================

# Ajouter une colonne 'Region' pour regrouper les pays
mydata$Region <- ifelse(mydata$Country %in% c("France", "Germany", "Belgium", "Netherlands", "UK", "EIRE"), 
                        "Europe",
                        ifelse(mydata$Country %in% c("Japan", "Australia", "New Zealand"), 
                               "Asie-Pacifique",
                               ifelse(mydata$Country %in% c("USA", "Canada"), 
                                      "Amérique",
                                      "Autres"))) # Autres pays regroupés dans 'Autres'

# Filtrer pour exclure la catégorie "Autres"
mydata_filtered <- mydata[mydata$Region != "Autres", ]

# Centrage et réduction des données pour les individus filtrés
numerical_data_filtered <- mydata_filtered[, c("Quantity", "UnitPrice")]
scaled_data_filtered <- scale(numerical_data_filtered)

# Appliquer l'ACP sur les données filtrées
resultats_ACP_filtered <- PCA(scaled_data_filtered, scale.unit = TRUE, ncp = 2, graph = FALSE)

# Définir une palette de couleurs pour les régions restantes
color_palette <- c("Europe" = "blue", "Asie-Pacifique" = "green", "Amérique" = "red")

# ===================== Étape 6 : Visualisations =====================

# A. Scree plot (Variance expliquée par les composantes)
barplot(resultats_ACP_filtered$eig[, 2], names.arg = 1:nrow(resultats_ACP_filtered$eig), 
        main = "Variance expliquée par les composantes", 
        xlab = "Composantes principales", ylab = "Pourcentage de variance", col = "skyblue")

# B. Graphique des individus avec habillage par région (sans 'Autres')
plot(resultats_ACP_filtered$ind$coord[, 1], resultats_ACP_filtered$ind$coord[, 2], 
     col = color_palette[mydata_filtered$Region], pch = 16,
     xlab = "Composante 1", ylab = "Composante 2", 
     main = "Individus (habillés par régions, sans 'Autres')")
grid()

# Ajouter la légende simplifiée
legend("topright", legend = names(color_palette), col = color_palette, 
       pch = 16, cex = 0.8, title = "Régions")

# C. Graphique des variables (projection dans l'espace ACP)
plot(resultats_ACP_filtered$var$coord[, 1], resultats_ACP_filtered$var$coord[, 2], 
     xlab = "Composante 1", ylab = "Composante 2", 
     main = "Graphique des variables (ACP)", col = "red", pch = 16)
arrows(0, 0, resultats_ACP_filtered$var$coord[, 1], resultats_ACP_filtered$var$coord[, 2], col = "red", length = 0.1)
text(resultats_ACP_filtered$var$coord[, 1], resultats_ACP_filtered$var$coord[, 2], 
     labels = rownames(resultats_ACP_filtered$var$coord), col = "black", cex = 0.8, pos = 3)

# D. Contribution des variables aux composantes principales
barplot(resultats_ACP_filtered$var$contrib[, 1:2], beside = TRUE, 
        col = c("lightblue", "orange"), 
        main = "Contribution des variables aux composantes",
        legend = c("Composante 1", "Composante 2"), las = 2)

# ===================== Étape 7 : Résumé des résultats =====================
print(resultats_ACP_filtered)
