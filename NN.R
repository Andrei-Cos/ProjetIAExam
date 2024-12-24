# Clear workspace and load required libraries
rm(list=ls())
library(class)      # for kNN
library(e1071)      # for SVM and Naive Bayes
library(mclust)     # for MClust
library(tree)       # for Decision Tree
library(caret)      # for confusion matrix
library(neuralnet)  # for Neural Network

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
index <- sample(1:N, size = round(N/3), replace = FALSE)

# Create training and test sets
training.set <- mydata_selected[-index, features]
training.class <- mydata_selected[-index, target]
test.set <- mydata_selected[index, features]
test.class <- mydata_selected[index, target]

########################################
# 4. Handling Imbalanced Data
########################################
# Combine training data with target
training.data <- cbind(training.set, TransactionSizeCategory = training.class)

# Oversample minority classes using caret's upSample
balanced.data <- upSample(x = training.set, y = training.class)

# Use the original test set for evaluation
training.set <- balanced.data[, -ncol(balanced.data)]
training.class <- as.factor(balanced.data$Class)

########################################
# 5. Data Normalization for Neural Network
########################################
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize numeric columns in training and test sets
training.set.nn <- as.data.frame(lapply(training.set, function(x) {
  if(is.numeric(x)) normalize(x) else as.numeric(x)
}))
test.set.nn <- as.data.frame(lapply(test.set, function(x) {
  if(is.numeric(x)) normalize(x) else as.numeric(x)
}))

# Add target variable back to training set
training.set.nn$TransactionSizeCategory <- as.numeric(training.class)

########################################
# 6. Neural Network Implementation and Evaluation
########################################
# Build the Neural Network
nn_model <- neuralnet(TransactionSizeCategory ~ Quantity + UnitPrice + TotalAmount +
                        ItemsPerTransaction + CustomerTotalPurchases +
                        CustomerTotalAmount + CustomerAverageAmount,
                      data = training.set.nn, hidden = c(10, 5), linear.output = FALSE, 
                      stepmax = 1e+06, lifesign = "full")

# Plot the Neural Network
plot(nn_model)

# Test the Neural Network
nn_results <- compute(nn_model, test.set.nn)

# Convert predictions to class labels
nn_predictions <- as.factor(round(nn_results$net.result))

# Evaluate the Neural Network
nn_cm <- confusionMatrix(nn_predictions, as.factor(as.numeric(test.class)))
cat("\nNeural Network Results (Original Test Data):\n")
print(nn_cm$table)
cat("Error Rate:", 1 - nn_cm$overall['Accuracy'], "\n")


