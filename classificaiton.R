# Clear workspace and load required libraries
rm(list=ls())
library(class)      # for kNN
library(e1071)      # for SVM and Naive Bayes
library(mclust)     # for MClust
library(tree)       # for Decision Tree
library(caret)      # for confusion matrix

########################################
# 1. Data Loading and Initial Processing
########################################
# Read the data
mydata <- read.csv("Online_Retail_Clean_Enhanced.csv")

# Convert categorical variables to factors
mydata$Country <- as.factor(mydata$Country)
mydata$TimeOfDay <- as.factor(mydata$TimeOfDay)
mydata$WeekPart <- as.factor(mydata$WeekPart)
mydata$TransactionSizeCategory <- as.factor(mydata$TransactionSizeCategory)

########################################
# 2. Data Cleaning
########################################
# Remove rows with NA, infinite, or negative values
mydata <- mydata[complete.cases(mydata), ]
numeric_cols <- sapply(mydata, is.numeric)
mydata <- mydata[apply(mydata[, numeric_cols], 1, function(x) !any(is.infinite(x))), ]
mydata <- mydata[apply(mydata[, numeric_cols], 1, function(x) all(x >= 0)), ]

# Select features
features <- c("Quantity", "UnitPrice", "TotalAmount", "ItemsPerTransaction",
              "CustomerTotalPurchases", "CustomerTotalAmount", "CustomerAverageAmount",
              "Country", "TimeOfDay", "WeekPart")
target <- "TransactionSizeCategory"

# Create dataset with selected features
mydata_selected <- mydata[, c(features, target)]

# Convert all character columns to factors
char_cols <- sapply(mydata_selected, is.character)
mydata_selected[, char_cols] <- lapply(mydata_selected[, char_cols], as.factor)

########################################
# 3. Data Splitting
########################################
set.seed(123)
N <- nrow(mydata_selected)
index <- sample(1:N, size = round(N/3), replace = FALSE) #Split 2/3 pour train et 1/3 pour comparer ces resultats

# Create training and test sets
training.set <- mydata_selected[-index, features]
training.class <- mydata_selected[-index, target]
test.set <- mydata_selected[index, features]
test.class <- mydata_selected[index, target]

########################################
# 4. Data Standardization
########################################
# Identify numeric columns
numeric_cols <- sapply(training.set, is.numeric)

# Standardization function
standardize <- function(x) {
  if(is.numeric(x)) {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
  return(x)
}

# Standardize numeric columns
training.set[, numeric_cols] <- as.data.frame(lapply(training.set[, numeric_cols], standardize))
test.set[, numeric_cols] <- as.data.frame(lapply(test.set[, numeric_cols], standardize))

# Convert factors to numeric for kNN
training.set_numeric <- data.frame(lapply(training.set, function(x) {
  if(is.factor(x)) as.numeric(x) else x
}))
test.set_numeric <- data.frame(lapply(test.set, function(x) {
  if(is.factor(x)) as.numeric(x) else x
}))

########################################
# 5. Model Implementation and Evaluation
########################################

# Evaluation function
evaluate_model <- function(pred, true_class, model_name) {
  cm <- confusionMatrix(pred, true_class)
  error_rate <- 1 - cm$overall['Accuracy']
  cat("\n", model_name, "Results:\n")
  print(cm$table)  # Display the confusion matrix
  cat("Error Rate:", error_rate, "\n")
  return(list(cm=cm, error_rate=error_rate))
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

# 5. Decision Tree with fix for high cardinality
# Function to handle high-cardinality factors
handle_high_cardinality <- function(data, column_name, top_n = 30) {
  freq_table <- table(data[[column_name]])
  top_categories <- names(sort(freq_table, decreasing = TRUE)[1:top_n])
  data[[column_name]] <- as.character(data[[column_name]])
  data[[column_name]][!(data[[column_name]] %in% top_categories)] <- "Other"
  data[[column_name]] <- as.factor(data[[column_name]])
  return(data)
}

# Create copies for decision tree
training.set.dt <- training.set
test.set.dt <- test.set

# Handle high-cardinality in Country column
training.set.dt <- handle_high_cardinality(training.set.dt, "Country")
test.set.dt <- handle_high_cardinality(test.set.dt, "Country")

# Create and evaluate decision tree model
dt_model <- tree(training.class ~ ., data = cbind(training.set.dt, training.class))
dt_pred <- predict(dt_model, test.set.dt, type = "class")
dt_results <- evaluate_model(dt_pred, test.class, "Decision Tree")

# Visualize Decision Tree
plot(dt_model)
text(dt_model, pretty = 0)

########################################
# 6. Compare Models
########################################
error_rates <- data.frame(
  Model = c("k-NN", "Naive Bayes", "SVM", "MClust", "Decision Tree"),
  Error_Rate = c(knn_results$error_rate, 
                 nb_results$error_rate,
                 svm_results$error_rate,
                 mclust_results$error_rate,
                 dt_results$error_rate)
)

# Print comparison
cat("\nModel Comparison (ordered by performance):\n")
print(error_rates[order(error_rates$Error_Rate),])

# Save results
write.csv(error_rates, "model_comparison_results.csv", row.names = FALSE)
