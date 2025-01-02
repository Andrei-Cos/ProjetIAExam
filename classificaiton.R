########################################
# Nettoyage de l'espace de travail et chargement des librairies
########################################
rm(list = ls())
library(class)      # kNN
library(e1071)      # SVM et Naive Bayes
library(mclust)     # MClust
library(tree)       # Arbre de décision
library(caret)      # Matrice de confusion

########################################
# 1. Chargement des données et prétraitement initial
########################################

# Lire le fichier CSV
mydata <- read.csv("Online_Retail_Clean_Enhanced.csv")

# Convertir les variables catégorielles en facteurs
mydata$Country <- as.factor(mydata$Country)
mydata$TimeOfDay <- as.factor(mydata$TimeOfDay)
mydata$WeekPart <- as.factor(mydata$WeekPart)
mydata$TransactionSizeCategory <- as.factor(mydata$TransactionSizeCategory)

########################################
# 2. Nettoyage des données
########################################

# Retirer les lignes contenant des NA ou des valeurs infinies/négatives
mydata <- mydata[complete.cases(mydata), ]
numeric_cols <- sapply(mydata, is.numeric)
mydata <- mydata[apply(mydata[, numeric_cols], 1, function(x) !any(is.infinite(x))), ]
mydata <- mydata[apply(mydata[, numeric_cols], 1, function(x) all(x >= 0)), ]

# Sélection des variables (features) et de la cible (target)
features <- c("Quantity", "UnitPrice", "TotalAmount", "ItemsPerTransaction",
              "CustomerTotalPurchases", "CustomerTotalAmount", "CustomerAverageAmount",
              "Country", "TimeOfDay", "WeekPart")
target <- "TransactionSizeCategory"

# Créer le jeu de données final
mydata_selected <- mydata[, c(features, target)]

# Convertir tout type "character" en facteur
char_cols <- sapply(mydata_selected, is.character)
mydata_selected[, char_cols] <- lapply(mydata_selected[, char_cols], as.factor)

########################################
# 3. Séparation des données en entraînement et test
########################################

set.seed(123)
N <- nrow(mydata_selected)
# On prélève 1/3 des données pour l’ensemble de test et 2/3 pour l’entraînement
index <- sample(1:N, size = round(N / 3), replace = FALSE)

# Créer les ensembles d’entraînement et de test
training.set <- mydata_selected[-index, features]
training.class <- mydata_selected[-index, target]
test.set <- mydata_selected[index, features]
test.class <- mydata_selected[index, target]

########################################
# 4. Standardisation des données
########################################

# Identifier les colonnes numériques
numeric_cols <- sapply(training.set, is.numeric)

# Fonction de standardisation
standardize <- function(x) {
  if (is.numeric(x)) {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
  return(x)
}

# Appliquer la standardisation aux variables numériques
training.set[, numeric_cols] <- as.data.frame(lapply(training.set[, numeric_cols], standardize))
test.set[, numeric_cols] <- as.data.frame(lapply(test.set[, numeric_cols], standardize))

# Convertir les facteurs en numérique pour k-NN
training.set_numeric <- data.frame(lapply(training.set, function(x) {
  if (is.factor(x)) as.numeric(x) else x
}))
test.set_numeric <- data.frame(lapply(test.set, function(x) {
  if (is.factor(x)) as.numeric(x) else x
}))

########################################
# 5. Implémentation et évaluation des modèles
########################################

# Fonction d’évaluation : calcule la matrice de confusion et le taux d’erreur
evaluate_model <- function(pred, true_class, model_name) {
  cm <- confusionMatrix(pred, true_class)
  error_rate <- 1 - cm$overall['Accuracy']
  cat("\n", model_name, " - Résultats:\n")
  print(cm$table)  # Affiche la matrice de confusion
  cat("Taux d'erreur:", error_rate, "\n")
  return(list(cm = cm, error_rate = error_rate))
}

# 1. k-NN
k <- 3
knn_pred <- knn(training.set_numeric, test.set_numeric, training.class, k = k)
knn_results <- evaluate_model(knn_pred, test.class, "k-NN")

# 2. Naive Bayes
nb_model <- naiveBayes(training.set, training.class)
nb_pred <- predict(nb_model, test.set)
nb_results <- evaluate_model(nb_pred, test.class, "Naive Bayes")

# 3. SVM
svm_model <- svm(x = training.set_numeric, y = training.class, kernel = "radial")
svm_pred <- predict(svm_model, test.set_numeric)
svm_results <- evaluate_model(svm_pred, test.class, "SVM")

# 4. MClust
mclust_model <- MclustDA(training.set_numeric, training.class)
mclust_pred <- predict(mclust_model, test.set_numeric)
mclust_results <- evaluate_model(as.factor(mclust_pred$classification), 
                                 test.class, "MClust")

# 5. Arbre de décision (Decision Tree) avec gestion de la forte cardinalité
# Fonction pour gérer la forte cardinalité
handle_high_cardinality <- function(data, column_name, top_n = 30) {
  freq_table <- table(data[[column_name]])
  top_categories <- names(sort(freq_table, decreasing = TRUE)[1:top_n])
  data[[column_name]] <- as.character(data[[column_name]])
  data[[column_name]][!(data[[column_name]] %in% top_categories)] <- "Other"
  data[[column_name]] <- as.factor(data[[column_name]])
  return(data)
}

# Créer des copies pour l'arbre de décision
training.set.dt <- training.set
test.set.dt <- test.set

# Gérer la cardinalité élevée pour la colonne "Country"
training.set.dt <- handle_high_cardinality(training.set.dt, "Country")
test.set.dt <- handle_high_cardinality(test.set.dt, "Country")

# Créer et évaluer le modèle d'arbre de décision
dt_model <- tree(training.class ~ ., data = cbind(training.set.dt, training.class))
dt_pred <- predict(dt_model, test.set.dt, type = "class")
dt_results <- evaluate_model(dt_pred, test.class, "Arbre de décision")

# Visualiser l'arbre de décision
plot(dt_model)
text(dt_model, pretty = 0)

########################################
# 6. Comparaison des modèles
########################################

error_rates <- data.frame(
  Model = c("k-NN", "Naive Bayes", "SVM", "MClust", "Arbre de décision"),
  Error_Rate = c(knn_results$error_rate, 
                 nb_results$error_rate,
                 svm_results$error_rate,
                 mclust_results$error_rate,
                 dt_results$error_rate)
)

# Afficher la comparaison
cat("\nComparaison des modèles (triés par performance):\n")
print(error_rates[order(error_rates$Error_Rate), ])

# Sauvegarder les résultats dans un fichier CSV
write.csv(error_rates, "model_comparison_results.csv", row.names = FALSE)
