# Load caret package if not already installed
# install.packages("caret")
library(caret)
library(randomForest)

# Ensure 'category' is a factor
df_selected$category <- as.factor(df_selected$category)

# Split data into training (80%) and testing (20%) sets
set.seed(rnorm(1))
accuracy <- 0

for (i in 1:100) {
  set.seed(rnorm(1))
  print(paste("Run Number: ", i))
  train_index <- sample(1:nrow(df_selected), 0.8 * nrow(df_selected))  # 80% for training
  train_data <- df_selected[train_index, ]
  test_data <- df_selected[-train_index, ]
  
  # Drop empty factor levels in 'category'
  train_data$category <- droplevels(train_data$category)
  
  # Train the Random Forest model
  model <- randomForest(category ~ ., data = train_data, ntree = 500, importance = TRUE)
  
  # Predict on test data
  predictions <- predict(model, newdata = test_data)
  
  # Confusion matrix
  conf_matrix <- confusionMatrix(predictions, test_data$category)
  
  # Extract accuracy
  accuracy <- accuracy + conf_matrix$overall['Accuracy']
}

accuracy <- accuracy/100
print(paste("Model Accuracy:", round(accuracy * 100, 2), "%"))