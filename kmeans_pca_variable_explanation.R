# ===================== Analyse complète : PCA + Clustering (version très optimisée) =====================
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)

# Augmenter la limite de mémoire vectorielle avant tout
memory.limit(size = 16000)
gc()  # Nettoyage initial de la mémoire

# Charger et préparer les données avec échantillonnage plus petit
set.seed(123)  
mydata <- read.csv("Online_Retail_Clean_Enhanced.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  sample_n(10000)  # Échantillon réduit à 10k observations

# Libérer immédiatement l'espace non utilisé
rm(list=setdiff(ls(), "mydata"))
gc()

library(factoextra)




# Première approche : Variables originales (avec nettoyage mémoire)
original_vars <- mydata %>% 
  select(Quantity, UnitPrice)
pca_original <- PCA(original_vars, scale.unit = TRUE, ncp = 2, graph = FALSE)
rm(original_vars)
gc()

# Deuxième approche : Variables dérivées
derived_vars <- mydata %>%
  select(Quantity, UnitPrice, TotalAmount, ItemsPerTransaction)
pca_derived <- PCA(derived_vars, scale.unit = TRUE, ncp = 4, graph = FALSE)

# Visualisation de la variance (en une seule fois)
par(mfrow = c(1, 2))
barplot(pca_original$eig[, 2], 
        main = "Variance expliquée\n(Variables originales)", 
        names.arg = paste("Dim", 1:nrow(pca_original$eig)),
        col = "lightblue",
        ylab = "Pourcentage de variance")
barplot(pca_derived$eig[, 2], 
        main = "Variance expliquée\n(Variables dérivées)", 
        names.arg = paste("Dim", 1:nrow(pca_derived$eig)),
        col = "lightgreen",
        ylab = "Pourcentage de variance")
par(mfrow = c(1, 1))

# Afficher les contributions
cat("\nContributions des variables (Approche originale):\n")
print(round(pca_original$var$contrib, 2))

cat("\nContributions des variables (Approche avec variables dérivées):\n")
print(round(pca_derived$var$contrib, 2))

# ===================== Analyse complète : PCA + Clustering (version très optimisée) =====================


# Nettoyage avant le clustering
rm(pca_original)
gc()

# Clustering basé sur PCA
pca_coords <- pca_derived$ind$coord[, 1:2]  # Garder seulement 2 dimensions
rm(pca_derived)
gc()


# Calcul de l'inertie pour différents K
fviz_nbclust(pca_coords, kmeans, method = "wss") +
  labs(title = "Méthode du coude", 
       x = "Nombre de clusters (K)", 
       y = "Inertie intra-classe") +
  theme_minimal()

# K-means directement
k <- 3
set.seed(123)
km_res <- kmeans(pca_coords, centers = k, nstart = 25)


# Statistiques des clusters
clustering_results <- derived_vars %>%
  mutate(Cluster = as.factor(km_res$cluster))

cluster_stats <- clustering_results %>%
  group_by(Cluster) %>%
  summarise(
    n = n(),
    avg_quantity = mean(Quantity),
    avg_price = mean(UnitPrice),
    avg_total = mean(TotalAmount),
    .groups = 'drop'
  ) %>%
  mutate(percentage = n/sum(n) * 100)

print("\nStatistiques des clusters:")
print(cluster_stats)




# Nettoyage final
gc()


# Définir les noms des clusters
cluster_names <- c("Gros Acheteurs en Volume", "Acheteurs Standards", "Acheteurs Premium")

# Créer la visualisation avec une seule légende
p <- fviz_cluster(km_res, data = pca_coords,
                  geom = "point",
                  ellipse.type = "convex",
                  palette = c("#0072B2", "#009E73", "#D55E00"),
                  main = "Segmentation des Clients par Profil d'Achat",
                  subtitle = "Basé sur l'analyse en composantes principales",
                  ggtheme = theme_minimal()) +
  labs(caption = "Dim.1 : Volume d'achat | Dim.2 : Valeur unitaire") +
  scale_color_manual(name = "Clusters",
                     values = c("#0072B2", "#009E73", "#D55E00"),
                     labels = cluster_names) +
  scale_shape_manual(name = "Clusters",
                     values = c(16, 17, 15),
                     labels = cluster_names) +
  # Supprimer la légende cluster et ajuster le style
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.position = "right",
        # Supprimer la légende fill
        legend.fill = element_blank()) +
  # Supprimer explicitement la légende de remplissage
  guides(fill = "none",
         color = guide_legend(override.aes = list(size = 4)))

print(p)
