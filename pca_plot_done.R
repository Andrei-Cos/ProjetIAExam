# Analyser les deux approches PCA
library(FactoMineR)
library(factoextra)


enhanced_data <- read.csv("Online_Retail_Clean_Enhanced.csv", header = TRUE, stringsAsFactors = FALSE)

# PCA avec variables originales
original_vars <- enhanced_data[, c("Quantity", "UnitPrice")]
pca_original <- PCA(original_vars, scale.unit = TRUE, ncp = 2, graph = FALSE)

# PCA avec variables dérivées
derived_vars <- enhanced_data[, c("Quantity", "UnitPrice", "TotalAmount", "ItemsPerTransaction")]
pca_derived <- PCA(derived_vars, scale.unit = TRUE, ncp = 4, graph = FALSE)

# Visualiser les contributions des variables pour chaque approche
# 1. Variables originales
fviz_pca_var(pca_original, 
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle des corrélations - Variables originales")

# 2. Variables dérivées
fviz_pca_var(pca_derived,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             title = "Cercle des corrélations - Variables dérivées")



# Afficher les contributions détaillées 
cat("\nContributions aux dimensions (Variables originales):\n")
print(round(pca_original$var$contrib, 2))

cat("\nContributions aux dimensions (Variables dérivées):\n")
print(round(pca_derived$var$contrib, 2))

# Afficher les corrélations avec les dimensions
cat("\nCorrélations avec les dimensions (Variables originales):\n")
print(round(pca_original$var$cor, 2))

cat("\nCorrélations avec les dimensions (Variables dérivées):\n")
print(round(pca_derived$var$cor, 2))